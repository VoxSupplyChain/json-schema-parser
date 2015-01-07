package json.schema.codegen

import java.net.URI

import json.schema.parser.SimpleType._
import json.schema.parser.{SchemaDocument, SimpleType}

import scalaz.Leibniz

import scalaz.Scalaz._

case class ScalaTypeProperty(name: String, required: Boolean, isa: ScalaType) {
  def propertyType = (required) ? isa.name | s"Option[${isa.name}]"
}

sealed trait ScalaType {
  val name: String

  def identifier = ScalaType.stringToIdentifier(name)
}


sealed case class ScalaScalar(name: String) extends ScalaType

sealed case class ScalaArray(unique: Boolean, nested: ScalaType) extends ScalaType {
  override val identifier = if (unique) s"Set[${nested.identifier}]" else s"List[${nested.identifier}]"
  val name = identifier
}

sealed case class ScalaClass(name: String, properties: List[ScalaTypeProperty]) extends ScalaType


object ScalaType {

  type Schema = SchemaDocument[Double]
  type Validation[T] = scalaz.Validation[String, T]

  val json2scala: Map[SimpleType, ScalaType] = Map(
    SimpleType.string -> ScalaScalar("String"),
    SimpleType.integer -> ScalaScalar("Int"),
    SimpleType.boolean -> ScalaScalar("Boolean"),
    // same as schema's document type param
    SimpleType.number -> ScalaScalar("Double"),
    SimpleType.`null` -> ScalaScalar("Any")
  )

  def stringToIdentifier(s: String) = s.map(c => c.isLetterOrDigit ? c | '_')


  def array(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    if (SimpleType.array == schema.types.headOption.orNull) {
      apply(schema.items.value.head, name.map(_ + "0")) map {
        nested =>
          ScalaArray(schema.uniqueItems, nested)
      }
    } else {
      s"unsupported schema type: ${schema.types}".fail
    }
  }


  def identifier(uri: URI) = {
    val str = uri.toString
    val lastSlash: Int = str.lastIndexOf('/')
    val lastSegment = (lastSlash >= 0) ? str.substring(lastSlash) | str
    stringToIdentifier(lastSegment.filter(c => c != '#'))
  }


  def underscoreToCamel(name: String) = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    m.group(1).toUpperCase()
  })

  def `object`(schema: Schema, name: Option[String]): Validation[ScalaType] = {

    implicit val ev = Leibniz.refl[Validation[ScalaTypeProperty]]

    if (SimpleType.`object` == schema.types.headOption.orNull) {

      val propertyTypes: List[scalaz.Validation[String, ScalaTypeProperty]] = schema.properties.value.map {
        case (name, prop) => apply(prop.schema, name.some) map {
          ScalaTypeProperty(name, prop.required, _)
        }
      }.toList

      propertyTypes.sequence.map {
        props =>
          val className: String = name.getOrElse(underscoreToCamel(identifier(schema.id))).capitalize
          ScalaClass(className, props)
      }

    } else {
      s"unsupported schema type: ${schema.types}".fail
    }
  }

  def apply(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    schema.types.headOption.map(json2scala.get).flatten.toSuccess("unknown type") orElse array(schema, name) orElse `object`(schema, name)
  }
}


