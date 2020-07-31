package json.schema.parser

import java.net.URI

import argonaut.{DecodeJson, DecodeResult, HCursor, Json}
import json.pointer.JsonPointer
import json.schema.parser.SimpleType.SimpleType

import scala.util.matching.Regex
import scalaz.{IList, NonEmptyList}
import json.schema.parser.JsonSchemaDecoder._

class JsonSchemaDecoder[N] protected (parentId: URI, rootSchema: Boolean)(implicit
    valueNumeric: Numeric[N],
    numberDecoder: DecodeJson[N]
) extends DecodeJson[SchemaDocument[N]]
    with Decoders {

  type Schema = SchemaDocument[N]

  private def validated[A](c: HCursor, successCondition: A => Boolean, err: => String): (A) => DecodeResult[A] = {
    v: A => if (successCondition(v)) DecodeResult.ok(v) else DecodeResult.fail(v.toString + err, c.history)
  }

  private def rangeConstrain(
      c: HCursor,
      minField: String,
      maxField: String
  ): DecodeResult[RangeConstrain[Inclusive[Int]]] =
    for {
      max <-
        c.get[Option[Int]](maxField)
          .flatMap(validated(c, _.forall(_ >= 0), s" must be greater or equal to 0"))
          .map(_.map(Inclusive.apply))
      min <-
        c.get[Option[Int]](minField)
          .flatMap(validated(c, _.forall(_ >= 0), s" must be greater or equal to 0"))
          .map(_.map(Inclusive.apply))
    } yield RangeConstrain(max, min.orElse(Some(Inclusive(0))))

  private def isPositive(a: N): Boolean = valueNumeric.compare(a, valueNumeric.zero) > 0

  private def nestedSchemas(c: HCursor)(nestedDocumentDecoder: DecodeJson[Schema]) =
    c.fieldSet
      .getOrElse(List.empty)
      .filterNot(schemaReservedFields.contains)
      .foldLeft(DecodeResult.ok(Map.empty[String, Schema])) { (result: DecodeResult[Map[String, Schema]], f: String) =>
        result flatMap { (r: Map[String, Schema]) =>
          val nestedDoc: DecodeResult[Schema] = c.get[Schema](f)(nestedDocumentDecoder)
          nestedDoc map ((d: Schema) => r + (f -> d))
        }
      }

  private def isValidId(uri: URI) = !uri.toString.isEmpty

  private def isValidSchema(uri: URI) = validSchemaVersions.contains(uri)

  private def numberType(c: HCursor): DecodeResult[NumberConstraint[N]] = {
    implicit val OptionalNumberDecoder: DecodeJson[Option[N]] = OptionDecodeJson(numberDecoder)

    for {
      multipleOf <-
        c.get[Option[N]]("multipleOf").flatMap(validated(c, _.fold(true)(isPositive), " must be positive number"))
      exclusiveMax <- c.get[Option[Boolean]]("exclusiveMaximum")
      exclusiveMin <- c.get[Option[Boolean]]("exclusiveMinimum")
      max          <- c.get[Option[N]]("maximum").map(_.map(Boundary[N](exclusiveMax.getOrElse(false), _)))
      min          <- c.get[Option[N]]("minimum").map(_.map(Boundary[N](exclusiveMin.getOrElse(false), _)))
    } yield NumberConstraint(multipleOf, RangeConstrain[Boundary[N]](max, min))
  }

  private def stringType(c: HCursor): DecodeResult[StringConstraint] =
    for {
      stringConstrain <- rangeConstrain(c, "minLength", "maxLength").option
      pattern         <- c.get[Option[Regex]]("pattern")
    } yield StringConstraint(stringConstrain.getOrElse(noIntConstain), pattern)

  private def arrayType(
      c: HCursor
  )(implicit nestedDocumentDecoder: DecodeJson[Schema]): DecodeResult[ArrayConstraint[N]] =
    for {
      additionalItems <- c.get[Option[Either[Boolean, Schema]]]("additionalItems")
      itemsConstrain  <- rangeConstrain(c, "minItems", "maxItems")
      items           <- c.get[Option[NonEmptyList[Schema]]]("items")(OptionDecodeJson(oneOrNonEmptyList(nestedDocumentDecoder)))
      uniqueItems     <- c.get[Option[Boolean]]("uniqueItems")
    } yield ArrayConstraint(
      additionalItems,
      ConstrainedList(items.fold(IList.empty[Schema])(_.list), itemsConstrain),
      uniqueItems.getOrElse(false)
    )

  private def objectType(c: HCursor, scope: URI)(implicit
      nestedDocumentDecoder: DecodeJson[Schema]
  ): DecodeResult[ObjectConstraint[N]] = {
    val mapOfSchemas = (c: HCursor, field: String) =>
      c.get[Option[Map[String, Schema]]](field)(OptionDecodeJson(MapDecodeJson))
    for {
      propsConstrain    <- rangeConstrain(c, "minProperties", "maxProperties")
      additionalProps   <- c.get[Option[Either[Boolean, Schema]]]("additionalProperties")
      properties        <- mapOfSchemas(c, "properties")
      patternProps      <- mapOfSchemas(c, "patternProperties")
      requiredPropNames <- c.get[Option[Set[String]]]("required")(OptionDecodeJson(nonEmptySetDecodeJsonStrict))
    } yield {
      val requiredField = requiredPropNames.getOrElse(Set.empty)

      ObjectConstraint[N](
        additionalProps.flatMap(
          _.fold[Option[Schema]](v => if (v) Some(SchemaDocument[N](scope)) else None, Option(_))
        ),
        ConstrainedMap(
          properties.getOrElse(Map.empty).map {
            case (name, schema) => name -> Property(requiredField.contains(name), schema)
          },
          propsConstrain
        ),
        patternProps.getOrElse(Map.empty).map {
          case (name, schema) => name.r -> schema
        }
      )
    }
  }

  def decode(c: HCursor): DecodeResult[Schema] = {
    val types: Set[SimpleType] = c.get[Set[SimpleType.SimpleType]]("type")(oneOrSetStrict).getOr(Set.empty[SimpleType])

    def when[T](t: SimpleType.SimpleType)(f: => DecodeResult[T]): DecodeResult[Option[T]] =
      if (types.contains(t)) f.option else DecodeResult.ok(None)

    for {
      // metadata
      id          <- c.get[Option[URI]]("id").flatMap(validated(c, _.forall(isValidId), " is not valid id"))
      title       <- c.get[Option[String]]("title")
      schema      <- c.get[Option[URI]]("$schema").flatMap(validated(c, _.forall(isValidSchema), " is not supported schema"))
      description <- c.get[Option[String]]("description")
      format      <- c.get[Option[String]]("format")
      // handy methods to decode common types
      scope: URI =
        if (rootSchema) id.getOrElse(parentId) else id.fold(parentId)(JsonPointer.resolveAsPointer(parentId, _))

      nestedDocumentDecoder: DecodeJson[Schema] = new JsonSchemaDecoder[N](parentId = scope, rootSchema = false)
      listOfSchemas = (c: HCursor, field: String) => {
        implicit val dec = oneOrNonEmptyList(nestedDocumentDecoder)
        c.get[Option[NonEmptyList[Schema]]](field).map(_.fold(IList.empty[Schema])(_.list))
      }
      mapOfSchemas = (c: HCursor, field: String) => {
        implicit val sc = nestedDocumentDecoder
        c.get[Option[Map[String, Schema]]](field)
      }
      // type constraints
      number <- when(SimpleType.number)(numberType(c))
      string <- when(SimpleType.string)(stringType(c))
      array  <- when(SimpleType.array)(arrayType(c)(nestedDocumentDecoder))
      obj    <- when(SimpleType.aObject)(objectType(c, scope)(nestedDocumentDecoder))
      // sub documents with reference to this document id
      definitions <- mapOfSchemas(c, "definitions")
      dependencies <- {
        implicit val dependencyDecoder = either(nestedDocumentDecoder, nonEmptySetDecodeJsonStrict[String])
        c.get[Option[Map[String, Either[Schema, Set[String]]]]]("dependencies")
      }
      // for any instance type
      enums <- {
        implicit val sd: DecodeJson[Set[Json]] = nonEmptySetDecodeJsonStrict
        c.get[Option[Set[Json]]]("enum")
      }
      anyOf <- listOfSchemas(c, "anyOf")
      allOf <- listOfSchemas(c, "allOf")
      oneOf <- listOfSchemas(c, "oneOf")
      not <- {
        implicit val dec = nestedDocumentDecoder
        c.get[Option[Schema]]("not")
      }
      nested <- nestedSchemas(c)(nestedDocumentDecoder)
    } yield SchemaDocument(
      scope,
      id,
      schema,
      number,
      string,
      array,
      obj,
      // common properties
      enums.getOrElse(Set.empty),
      nested,
      title,
      description,
      format,
      // additional sub types
      definitions.getOrElse(Map.empty),
      dependencies.getOrElse(Map.empty),
      types,
      anyOf,
      allOf,
      oneOf,
      not
    )
  }
}

object JsonSchemaDecoder {

  private val noIntConstain = RangeConstrain[Inclusive[Int]]()

  private val schemaReservedFields = Set(
    "id",
    "title",
    "$schema",
    "description",
    "default",
    "multipleOf",
    "maximum",
    "exclusiveMaximum",
    "minimum",
    "exclusiveMinimum",
    "maxLength",
    "minLength",
    "pattern",
    "additionalItems",
    "items",
    "maxItems",
    "minItems",
    "uniqueItems",
    "maxProperties",
    "minProperties",
    "required",
    "additionalProperties",
    "definitions",
    "properties",
    "patternProperties",
    "dependencies",
    "enum",
    "type",
    "allOf",
    "anyOf",
    "oneOf",
    "not"
  )

  private val validSchemaVersions =
    Set(new URI("http://json-schema.org/schema#"), new URI("http://json-schema.org/draft-04/schema#"))

  def apply[N: Numeric: DecodeJson](parentId: URI = new URI("#")) = new JsonSchemaDecoder(parentId, rootSchema = true)
}
