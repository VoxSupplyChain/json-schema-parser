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

  case class SchemaCommon[N](
                              title: Option[String],
                              description: Option[String],
                              format: Option[String],

                              definitions: Map[String, SchemaDocument[N]],
                              dependencies: Map[String, Either[SchemaDocument[N], Set[String]]],
                              types: Set[SimpleType.SimpleType],
                              anyOf: List[SchemaDocument[N]],
                              allOf: List[SchemaDocument[N]],
                              oneOf: List[SchemaDocument[N]],
                              not: Option[SchemaDocument[N]]
                              )

  case class SchemaDocument[N](
                                id: Option[URI],
                                scope: URI,
                                schema: Option[URI],
                                // number
                                multipleOf: Option[N],
                                valueConstraint: RangeConstrain[Exclusivity[N]],
                                // string
                                stringConstraint: RangeConstrain[Int],
                                pattern: Option[Regex],
                                // array
                                additionalItems: Option[Either[Boolean, SchemaDocument[N]]],
                                items: ConstrainedList[SchemaDocument[N]],
                                uniqueItems: Boolean,
                                // object
                                additionalProperties: Option[SchemaDocument[N]],
                                properties: ConstrainedMap[Property[N]],
                                patternProperties: Map[Regex, SchemaDocument[N]],
                                // enumeration
                                enums: Set[Json],
                                // common
                                common: SchemaCommon[N],
                                nestedSchemas: Map[String, SchemaDocument[N]]

                                ) {
    override def toString: String = {
      val props = properties.value.keys
      val key = id.getOrElse(scope)
      s"JsonSchema($key -> $props)"
    }
  }

  object SchemaDocument {
    def empty[N](scope: URI)(implicit n: Numeric[N]) = {
      val noConstraintE: RangeConstrain[Exclusivity[N]] = RangeConstrain(None, None)
      val noConstraint: RangeConstrain[Int] = RangeConstrain(None, None)
      SchemaDocument(
        None, scope, None, None, noConstraintE, noConstraint, None, None, ConstrainedList[SchemaDocument[N]](Nil, noConstraint), uniqueItems = false, None, ConstrainedMap[Property[N]](Map.empty, noConstraint), Map.empty, Set.empty,
        SchemaCommon(None, None, None, Map.empty, Map.empty, Set.empty, Nil, Nil, Nil, None), Map.empty
      )
    }

  }


}