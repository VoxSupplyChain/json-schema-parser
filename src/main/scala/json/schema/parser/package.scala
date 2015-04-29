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

  sealed trait Boundary[N] {
    def above(v: N)(implicit n: Numeric[N]): Boolean

    def below(v: N)(implicit n: Numeric[N]): Boolean
  }

  case class Exclusive[N](value: N) extends Boundary[N] {
    override def above(v: N)(implicit n: Numeric[N]): Boolean = n.gt(value, v)

    override def below(v: N)(implicit n: Numeric[N]): Boolean = n.lt(value, v)
  }

  case class Inclusive[N](value: N) extends Boundary[N] {
    override def above(v: N)(implicit n: Numeric[N]): Boolean = n.gteq(value, v)

    override def below(v: N)(implicit n: Numeric[N]): Boolean = n.lteq(value, v)
  }

  object Boundary {
    def apply[N](exclusive: Boolean, v: N): Boundary[N] = if (exclusive) Exclusive(v) else Inclusive(v)
  }

  case class RangeConstrain[T](max: Option[T] = None, min: Option[T] = None)

  case class ConstrainedList[T](value: List[T], sizeConstrain: RangeConstrain[Inclusive[Int]])

  case class ConstrainedMap[T](value: Map[String, T], sizeConstrain: RangeConstrain[Inclusive[Int]])

  case class Property[N](required: Boolean, schema: SchemaDocument[N])

  case class NumberConstraint[N](multipleOf: Option[N], valueConstraint: RangeConstrain[Boundary[N]])

  case class StringConstraint(stringConstraint: RangeConstrain[Inclusive[Int]], pattern: Option[Regex])

  case class ArrayConstraint[N](additionalItems: Option[Either[Boolean, SchemaDocument[N]]], items: ConstrainedList[SchemaDocument[N]], uniqueItems: Boolean)

  case class ObjectConstraint[N](
                                  additionalProperties: Option[SchemaDocument[N]],
                                  properties: ConstrainedMap[Property[N]],
                                  patternProperties: Map[Regex, SchemaDocument[N]]
                                  )

  case class SchemaDocument[N](
                                id: Option[URI],
                                scope: URI,
                                schema: Option[URI],
                                // type specific constraints
                                number: Option[NumberConstraint[N]],
                                string: Option[StringConstraint],
                                array: Option[ArrayConstraint[N]],
                                obj: Option[ObjectConstraint[N]],

                                // common
                                enums: Set[Json],
                                nestedSchemas: Map[String, SchemaDocument[N]],

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

                                ) {
    override def toString: String = {
      val props = obj.map(_.properties.value.keys).getOrElse(Nil)
      val key = id.getOrElse(scope)
      s"JsonSchema($key -> $props)"
    }
  }

  object SchemaDocument {
    def empty[N](scope: URI)(implicit n: Numeric[N]): SchemaDocument[N] = {

      SchemaDocument(
        None, scope, None,
        None, None,
        None, None,
        Set.empty,
        Map.empty,
        None, None, None, Map.empty, Map.empty, Set.empty, Nil, Nil, Nil, None
      )
    }

  }

}
