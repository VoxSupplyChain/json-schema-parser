package json.schema.parser


import java.net.URI

import argonaut.{DecodeJson, DecodeResult, HCursor, Json}

import scala.util.matching.Regex


class JsonSchemaDecoderFactory[N](valueNumeric: Numeric[N], numberDecoder: DecodeJson[N]) extends URIResolver[N] {

  type Schema = SchemaDocument[N]
  
  import JsonSchemaDecoderFactory._
  import SchemaDecoders._

  private def validated[A](c: HCursor, successCondition: A => Boolean, err: => String): (A) => DecodeResult[A] = {
    v: A => if (successCondition(v)) DecodeResult.ok(v) else DecodeResult.fail(v.toString + err, c.history)
  }

  private def rangeConstrain(c: HCursor, minField: String, maxField: String): DecodeResult[RangeConstrain[Int]] = for {
    max <- c.get[Int](maxField).flatMap(validated(c, _ >= 0, s" must be greater or equal to 0")).option
    min <- c.get[Int](minField).flatMap(validated(c, _ >= 0, s" must be greater or equal to 0")).option
  } yield RangeConstrain(max, min.orElse(Some(0)))


  private def isPositive(a: N) = valueNumeric.compare(a, valueNumeric.zero) > 0

  private def nestedSchemaFields(c: HCursor) = c.fieldSet.getOrElse(List.empty).filterNot(schemaFields.contains).toSet

  private def isValidId(uri: URI) = uri.toString != "#" && !uri.toString.isEmpty

  private def isValidSchema(uri: URI) = schemaVersions.contains(uri)

  def apply(parentId: URI, rootSchema:Boolean): DecodeJson[Schema] = DecodeJson { c =>

    implicit val OptionalNumberDecoder = OptionDecodeJson(numberDecoder)

    for {
    // metadata
      id <- c.get[Option[URI]]("id").flatMap(validated(c, _.map(isValidId).getOrElse(true), " is not valid id"))
      title <- c.get[Option[String]]("title")
      schema <- c.get[Option[URI]]("$schema").flatMap(validated(c, _.map(isValidSchema).getOrElse(true), " is not supported schema"))
      description <- c.get[Option[String]]("description")
      format <- c.get[Option[String]]("format")

      // sub documents with reference to this document id
      resolvedId: URI = if (rootSchema) id.getOrElse(parentId) else id.map( resolve(parentId, _)).getOrElse(parentId)
      nestedDocumentDecoder = apply(resolvedId, rootSchema=false)

      // handy methods to decode common types
      listOfSchemas = (c: HCursor, field: String) => c.get[List[Schema]](field)(oneOrNonEmptyList(nestedDocumentDecoder)).option
      mapOfSchemas = (c: HCursor, field: String) => c.get[Map[String, Schema]](field)(MapDecodeJson(nestedDocumentDecoder)).option
      dependencyDecoder = either(nestedDocumentDecoder, NonEmptySetDecodeJsonStrict[String])
      additionalDecoder = either(implicitly[DecodeJson[Boolean]], nestedDocumentDecoder)

      // value constrains
      multipleOf <- c.get[Option[N]]("multipleOf").flatMap(validated(c, _.map(isPositive).getOrElse(true), " must be positive number"))
      exclusiveMax <- c.get[Option[Boolean]]("exclusiveMaximum")
      exclusiveMin <- c.get[Option[Boolean]]("exclusiveMinimum")
      max <- c.get[Option[N]]("maximum").map(_.map(Exclusivity[N](exclusiveMax.getOrElse(false), _)))
      min <- c.get[Option[N]]("minimum").map(_.map(Exclusivity[N](exclusiveMin.getOrElse(false), _)))
      // string constrains
      stringConstrain <- rangeConstrain(c, "minLength", "maxLength").option
      pattern <- c.get[Option[Regex]]("pattern")
      // array constrains
      additionalItems <- c.get[Either[Boolean, Schema]]("additionalItems")(additionalDecoder).option
      itemsConstrain <- rangeConstrain(c, "minItems", "maxItems").option
      items <- c.get[List[Schema]]("items")(oneOrNonEmptyList(nestedDocumentDecoder)).option
      uniqueItems <- c.get[Option[Boolean]]("uniqueItems")
      // object constrains
      propsConstrain <- rangeConstrain(c, "minProperties", "maxProperties").option
      additionalProps <- c.get[Either[Boolean, Schema]]("additionalProperties")(additionalDecoder).option
      properties <- mapOfSchemas(c, "properties")
      patternProps <- mapOfSchemas(c, "patternProperties")
      requiredPropNames <- c.get[Set[String]]("required")(NonEmptySetDecodeJsonStrict).option
      definitions <- mapOfSchemas(c, "definitions")
      dependencies <- c.get[Map[String, Either[Schema, Set[String]]]]("dependencies")(MapDecodeJson(dependencyDecoder)).option
      // for any instance type
      enums <- c.get[Set[Json]]("enum")(NonEmptySetDecodeJsonStrict).option
      types <- c.get[Set[SimpleType.SimpleType]]("type")(oneOrSetStrict).option
      anyOf <- listOfSchemas(c, "anyOf")
      allOf <- listOfSchemas(c, "allOf")
      oneOf <- listOfSchemas(c, "oneOf")
      not <- c.get[Schema]("not")(nestedDocumentDecoder).option
      nested <- nestedSchemaFields(c).foldLeft(DecodeResult.ok(Map.empty[String, Schema])) {
        (result: DecodeResult[Map[String, Schema]], f: String) =>
          result flatMap {
            (r: Map[String, Schema]) =>
              val nestedDoc: DecodeResult[Schema] = c.get[Schema](f)(nestedDocumentDecoder)
              nestedDoc map ((d: Schema) => r + (f -> d))
          }
      }

    } yield {
      val requiredField = requiredPropNames.getOrElse(Set.empty)
      new SchemaDocument(
        resolvedId,
        schema, title,
        description, format,
        multipleOf, RangeConstrain[Exclusivity[N]](max, min),
        stringConstrain.getOrElse(noIntConstain), pattern,
        additionalItems, ConstrainedList(items.getOrElse(List.empty), itemsConstrain.getOrElse(noIntConstain)), uniqueItems.getOrElse(false),
        additionalProps,
        ConstrainedMap(
          properties.getOrElse(Map.empty).map((kv) => kv._1 -> Property(requiredField.contains(kv._1), kv._2)).toMap,
          propsConstrain.getOrElse(noIntConstain)),
        patternProps.getOrElse(Map.empty).map((kv) => kv._1.r -> kv._2).toMap,
        definitions.getOrElse(Map.empty),
        dependencies.getOrElse(Map.empty),
        enums.getOrElse(Set.empty),
        types.getOrElse(Set.empty),
        anyOf.getOrElse(List.empty), allOf.getOrElse(List.empty), oneOf.getOrElse(List.empty),
        not,
        nested
      )
    }
  }
}

object JsonSchemaDecoderFactory {

  private val noIntConstain = RangeConstrain[Int]()

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

  def apply[N](implicit valueNumeric: Numeric[N], numberDecoder: DecodeJson[N]): DecodeJson[JsonSchemaDecoderFactory[N]#Schema] = new JsonSchemaDecoderFactory(valueNumeric, numberDecoder).apply(new URI("#"), rootSchema=true)
}