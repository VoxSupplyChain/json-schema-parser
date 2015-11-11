package json.schema.parser


import java.net.URI

import argonaut.{DecodeJson, DecodeResult, HCursor, Json}
import json.reference.ReferenceResolver
import json.schema.parser.SimpleType.SimpleType

import scala.util.matching.Regex


class JsonSchemaDecoderFactory[N](valueNumeric: Numeric[N], numberDecoder: DecodeJson[N]) {

  type Schema = SchemaDocument[N]

  import json.schema.parser.JsonSchemaDecoderFactory._
  import json.schema.parser.SchemaDecoders._

  private def validated[A](c: HCursor, successCondition: A => Boolean, err: => String): (A) => DecodeResult[A] = {
    v: A => if (successCondition(v)) DecodeResult.ok(v) else DecodeResult.fail(v.toString + err, c.history)
  }

  private def rangeConstrain(c: HCursor, minField: String, maxField: String): DecodeResult[RangeConstrain[Inclusive[Int]]] = for {
    max <- c.get[Int](maxField).flatMap(validated(c, _ >= 0, s" must be greater or equal to 0")).map(Inclusive(_)).option
    min <- c.get[Int](minField).flatMap(validated(c, _ >= 0, s" must be greater or equal to 0")).map(Inclusive(_)).option
  } yield RangeConstrain(max, min.orElse(Some(Inclusive(0))))


  private def isPositive(a: N): Boolean = valueNumeric.compare(a, valueNumeric.zero) > 0

  private def nestedSchemas(c: HCursor)(nestedDocumentDecoder: DecodeJson[Schema]) =
    c.fieldSet.getOrElse(List.empty).filterNot(schemaFields.contains).foldLeft(DecodeResult.ok(Map.empty[String, Schema])) {
      (result: DecodeResult[Map[String, Schema]], f: String) =>
        result flatMap {
          (r: Map[String, Schema]) =>
            val nestedDoc: DecodeResult[Schema] = c.get[Schema](f)(nestedDocumentDecoder)
            nestedDoc map ((d: Schema) => r + (f -> d))
        }
    }

  private def isValidId(uri: URI) = !uri.toString.isEmpty

  private def isValidSchema(uri: URI) = schemaVersions.contains(uri)


  private def numberType(c: HCursor): DecodeResult[NumberConstraint[N]] = {
    implicit val OptionalNumberDecoder: DecodeJson[Option[N]] = OptionDecodeJson(numberDecoder)

    for {
      multipleOf <- c.get[Option[N]]("multipleOf").flatMap(validated(c, _.map(isPositive).getOrElse(true), " must be positive number"))
      exclusiveMax <- c.get[Option[Boolean]]("exclusiveMaximum")
      exclusiveMin <- c.get[Option[Boolean]]("exclusiveMinimum")
      max <- c.get[Option[N]]("maximum").map(_.map(Boundary[N](exclusiveMax.getOrElse(false), _)))
      min <- c.get[Option[N]]("minimum").map(_.map(Boundary[N](exclusiveMin.getOrElse(false), _)))
    } yield NumberConstraint(multipleOf, RangeConstrain[Boundary[N]](max, min))
  }

  private def stringType(c: HCursor): DecodeResult[StringConstraint] = for {
    stringConstrain <- rangeConstrain(c, "minLength", "maxLength").option
    pattern <- c.get[Option[Regex]]("pattern")
  } yield StringConstraint(stringConstrain.getOrElse(noIntConstain), pattern)

  private def arrayType(c: HCursor)(nestedDocumentDecoder: DecodeJson[Schema]): DecodeResult[ArrayConstraint[N]] = {
    val additionalDecoder = either(implicitly[DecodeJson[Boolean]], nestedDocumentDecoder)

    for {
      additionalItems <- c.get[Either[Boolean, Schema]]("additionalItems")(additionalDecoder).option
      itemsConstrain <- rangeConstrain(c, "minItems", "maxItems").option
      items <- c.get[List[Schema]]("items")(oneOrNonEmptyList(nestedDocumentDecoder)).option
      uniqueItems <- c.get[Option[Boolean]]("uniqueItems")
    } yield ArrayConstraint(
      additionalItems, ConstrainedList(items.getOrElse(List.empty),
        itemsConstrain.getOrElse(noIntConstain)), uniqueItems.getOrElse(false)
    )
  }

  private def objectType(c: HCursor, scope: URI)(implicit nestedDocumentDecoder: DecodeJson[Schema]) = {
    val mapOfSchemas = (c: HCursor, field: String) => c.get[Map[String, Schema]](field)(MapDecodeJson).option
    val additionalDecoder = either(implicitly[DecodeJson[Boolean]], nestedDocumentDecoder)
    for {
      propsConstrain <- rangeConstrain(c, "minProperties", "maxProperties").option
      additionalProps <- c.get[Either[Boolean, Schema]]("additionalProperties")(additionalDecoder).option
      properties <- mapOfSchemas(c, "properties")
      patternProps <- mapOfSchemas(c, "patternProperties")
      requiredPropNames <- c.get[Set[String]]("required")(nonEmptySetDecodeJsonStrict).option
    } yield {
      val requiredField = requiredPropNames.getOrElse(Set.empty)

      ObjectConstraint(
        additionalProps.flatMap(_.fold(v => if (v) Some(SchemaDocument.empty(scope)(valueNumeric)) else None, Some(_))),
        ConstrainedMap(
          properties.getOrElse(Map.empty).map((kv) => kv._1 -> Property(requiredField.contains(kv._1), kv._2)),
          propsConstrain.getOrElse(noIntConstain)),
        patternProps.getOrElse(Map.empty).map((kv) => kv._1.r -> kv._2)
      )
    }
  }

  def apply(parentId: URI, rootSchema: Boolean): DecodeJson[Schema] = DecodeJson { c =>
    val types: Set[SimpleType] = c.get[Set[SimpleType.SimpleType]]("type")(oneOrSetStrict).getOr(Set.empty[SimpleType])
    def when[T](t: SimpleType.SimpleType)(f: => DecodeResult[T]): DecodeResult[Option[T]] = if (types.contains(t)) f.option else DecodeResult.ok(None)

    for {
    // metadata
      id <- c.get[Option[URI]]("id").flatMap(validated(c, _.map(isValidId).getOrElse(true), " is not valid id"))
      title <- c.get[Option[String]]("title")
      schema <- c.get[Option[URI]]("$schema").flatMap(validated(c, _.map(isValidSchema).getOrElse(true), " is not supported schema"))
      description <- c.get[Option[String]]("description")
      format <- c.get[Option[String]]("format")
      // handy methods to decode common types
      scope: URI = if (rootSchema) id.getOrElse(parentId) else id.map(ReferenceResolver.resolve(parentId, _)).getOrElse(parentId)

      nestedDocumentDecoder: DecodeJson[Schema] = apply(scope, rootSchema = false)
      additionalDecoder = either(implicitly[DecodeJson[Boolean]], nestedDocumentDecoder)
      listOfSchemas = (c: HCursor, field: String) => c.get[List[Schema]](field)(oneOrNonEmptyList(nestedDocumentDecoder)).option
      mapOfSchemas = (c: HCursor, field: String) => {
        implicit val sc = nestedDocumentDecoder
        c.get[Map[String, Schema]](field).option
      }
      // type constraints
      number <- when(SimpleType.number)(numberType(c))
      string <- when(SimpleType.string)(stringType(c))
      array <- when(SimpleType.array)(arrayType(c)(nestedDocumentDecoder))
      obj <- when(SimpleType.`object`)(objectType(c, scope)(nestedDocumentDecoder))
      // sub documents with reference to this document id
      definitions <- mapOfSchemas(c, "definitions")
      dependencies <- {
        implicit val dependencyDecoder = either(nestedDocumentDecoder, nonEmptySetDecodeJsonStrict[String])
        c.get[Map[String, Either[Schema, Set[String]]]]("dependencies").option
      }
      // for any instance type
      enums <- c.get[Set[Json]]("enum")(nonEmptySetDecodeJsonStrict).option
      anyOf <- listOfSchemas(c, "anyOf")
      allOf <- listOfSchemas(c, "allOf")
      oneOf <- listOfSchemas(c, "oneOf")
      not <- c.get[Schema]("not")(nestedDocumentDecoder).option
      nested <- nestedSchemas(c)(nestedDocumentDecoder)
    } yield {
      SchemaDocument(
        id, scope, schema,
        number, string, array, obj,
        // common properties
        enums.getOrElse(Set.empty), nested,
        title, description, format,
        // additional sub types
        definitions.getOrElse(Map.empty), dependencies.getOrElse(Map.empty), types,
        anyOf.getOrElse(List.empty), allOf.getOrElse(List.empty), oneOf.getOrElse(List.empty), not
      )
    }
  }
}

object JsonSchemaDecoderFactory {

  private val noIntConstain = RangeConstrain[Inclusive[Int]]()

  private val schemaFields = Set(
    "id", "title", "$schema",
    "description", "default",
    "multipleOf",
    "maximum", "exclusiveMaximum",
    "minimum", "exclusiveMinimum",
    "maxLength", "minLength",
    "pattern",
    "additionalItems",
    "items", "maxItems", "minItems",
    "uniqueItems", "maxProperties", "minProperties",
    "required", "additionalProperties", "definitions",
    "properties", "patternProperties",
    "dependencies",
    "enum", "type",
    "allOf", "anyOf", "oneOf",
    "not"
  )

  private val schemaVersions = Set(new URI("http://json-schema.org/schema#"), new URI("http://json-schema.org/draft-04/schema#"))

  def apply[N](uri: URI = new URI("#"))(implicit valueNumeric: Numeric[N], numberDecoder: DecodeJson[N]): DecodeJson[JsonSchemaDecoderFactory[N]#Schema] =
    new JsonSchemaDecoderFactory(valueNumeric, numberDecoder).apply(uri, rootSchema = true)
}
