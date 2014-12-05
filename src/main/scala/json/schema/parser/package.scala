package json.schema

import java.net.URI

import argonaut.Json

import scala.util.matching.Regex


package object parser {

  object SimpleType extends Enumeration {
    type SimpleType = Value
    val array, boolean, integer, number, string = Value
    val `null` = Value("null")
    val `object` = Value("object")
  }

  object Format extends Enumeration {
    type Format = Value
    val email, hostname, ipv4, ipv6, uri, regex = Value
    val `date-time` = Value("date-time")
  }

  type SchemaRef = String

  sealed trait Exclusivity[N]

  case class Exclusive[N](value: N) extends Exclusivity[N]

  case class Inclusive[N](value: N) extends Exclusivity[N]

  object Exclusivity {
    def apply[N](exclusive: Boolean, v: N): Exclusivity[N] = if (exclusive) Exclusive(v) else Inclusive(v)
  }

  case class RangeConstrain[T](max: Option[T] = None, min: Option[T] = None)

  case class ConstrainedList[T](value: List[T], sizeConstrain: RangeConstrain[Int])

  case class ConstrainedMap[T](value: Map[String, T], sizeConstrain: RangeConstrain[Int])

  case class Property[N](required: Boolean, schema: SchemaDocument[N])

  case class SchemaReference(uri: URI)

  class SchemaDocument[N](
                              val id: URI,
                              val schema: Option[URI],
                              val title: Option[String],
                              val description: Option[String],
                              val format: Option[String],
                              // number
                              val multipleOf: Option[N],
                              val valueConstraint: RangeConstrain[Exclusivity[N]],
                              // string
                              val stringConstraint: RangeConstrain[Int],
                              val pattern: Option[Regex],
                              // array
                              val additionalItems: Option[Either[Boolean, SchemaDocument[N]]],
                              val items: ConstrainedList[SchemaDocument[N]],
                              val uniqueItems: Boolean,
                              // object
                              val additionalProperties: Option[Either[Boolean, SchemaDocument[N]]],
                              val properties: ConstrainedMap[Property[N]],
                              val patternProperties: Map[Regex, SchemaDocument[N]],
                              // common
                              val definitions: Map[String, SchemaDocument[N]],
                              val dependencies: Map[String, Either[SchemaDocument[N], Set[String]]],
                              val enums: Set[Json],
                              val types: Set[SimpleType.SimpleType],
                              val anyOf: List[SchemaDocument[N]],
                              val allOf: List[SchemaDocument[N]],
                              val oneOf: List[SchemaDocument[N]],
                              val not: Option[SchemaDocument[N]],
                              val nestedSchemas: Map[String, SchemaDocument[N]]
                              ) {
    override def toString: String = s"Schema[$id]"
  }


}