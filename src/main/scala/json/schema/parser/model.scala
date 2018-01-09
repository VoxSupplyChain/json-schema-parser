package json.schema.parser

import java.net.URI

import argonaut.Json

import scala.util.matching.Regex
import scalaz.{IList, NonEmptyList}


object SimpleType extends Enumeration {
  type SimpleType = Value
  val array, boolean, integer, number, string = Value
  val aNull = Value("null")
  val aObject = Value("object")
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

case class ConstrainedList[T](value: IList[T], sizeConstrain: RangeConstrain[Inclusive[Int]])

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
                              scope: URI,
                              id: Option[URI] = None,
                              schema: Option[URI] = None,
                              // type specific constraints
                              number: Option[NumberConstraint[N]] = None,
                              string: Option[StringConstraint] = None,
                              array: Option[ArrayConstraint[N]] = None,
                              obj: Option[ObjectConstraint[N]] = None,

                              // common
                              enums: Set[Json] = Set.empty,
                              nestedSchemas: Map[String, SchemaDocument[N]] = Map.empty[String, SchemaDocument[N]],

                              title: Option[String] = None,
                              description: Option[String] = None,

                              format: Option[String] = None,

                              definitions: Map[String, SchemaDocument[N]] = Map.empty[String, SchemaDocument[N]],
                              dependencies: Map[String, Either[SchemaDocument[N], Set[String]]] = Map.empty[String, Either[SchemaDocument[N], Set[String]]],
                              types: Set[SimpleType.SimpleType] = Set.empty,
                              anyOf: IList[SchemaDocument[N]] = IList.empty[SchemaDocument[N]],
                              allOf: IList[SchemaDocument[N]] = IList.empty[SchemaDocument[N]],
                              oneOf: IList[SchemaDocument[N]] = IList.empty[SchemaDocument[N]],
                              not: Option[SchemaDocument[N]] = None

                              ) {
  override def toString: String = {
    val key = id.getOrElse(scope)
    s"JsonSchema($key)"
  }
}

