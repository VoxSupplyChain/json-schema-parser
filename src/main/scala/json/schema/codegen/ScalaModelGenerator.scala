package json.schema.codegen

import java.net.URI

import json.schema.parser.SimpleType._
import json.schema.parser.{SchemaDocument, SimpleType}

import scala.collection.mutable
import scalaz.Leibniz
import scalaz.Scalaz._

private class ScalaModelGenerator {

  import ScalaModelGenerator._

  val types = mutable.Map.empty[Schema, ScalaClass]

  def identifier(uri: URI): String = {
    val str = uri.toString
    val lastSlash: Int = str.lastIndexOf('/')
    val lastSegment = (lastSlash >= 0) ? str.substring(lastSlash) | str
    identifier(lastSegment.filter(c => c != '#'))
  }

  def identifier(s: String) = s.map(c => c.isLetterOrDigit ? c | '_')

  def underscoreToCamel(name: String): String = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    m.group(1).toUpperCase
  })

  def `object`(schema: Schema, name: Option[String]): Validation[ScalaType] = {

    implicit val ev = Leibniz.refl[Validation[ScalaTypeProperty]]

    if (SimpleType.`object` == schema.common.types.headOption.orNull) {

      val propertyTypes: List[Validation[ScalaTypeProperty]] = schema.properties.value.map {
        case (n, prop) =>

          val existingType = types.get(prop.schema).toSuccess("no type")

          existingType orElse any(prop.schema, n.some) map {
            t =>
              ScalaTypeProperty(n, prop.required, t)
          }

      }.toList

      for {
        props <- propertyTypes.sequence
        className <- schema.id.map(u => underscoreToCamel(identifier(u))).orElse(name.map(s => underscoreToCamel(identifier(s)))).map(_.capitalize).toSuccess(s"Can not name $schema")

      } yield {
        val newType = ScalaClass(className, props)
        types.put(schema, newType)
        newType
      }

    } else {
      s"unsupported schema type: ${schema.common.types}".fail
    }
  }


  def array(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    if (SimpleType.array == schema.common.types.headOption.orNull) {
      any(schema.items.value.head, name.map(_ + "0")) map {
        nested =>
          ScalaArray(schema.uniqueItems, nested)
      }
    } else {
      s"unsupported schema type: ${schema.common.types}".fail
    }
  }

  def scalar(schema: Schema): Validation[ScalaType] = {
    schema.common.types.headOption.map(json2scala.get).flatten.toSuccess("unknown type")
  }

  def any(schema: Schema, name: Option[String]): Validation[ScalaType] = {
    scalar(schema) orElse array(schema, name) orElse `object`(schema, name)
  }

}

object ScalaModelGenerator {


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


  def apply(schema: Schema, name: Option[String]) = {
    val generator: ScalaModelGenerator = new ScalaModelGenerator()
    generator.any(schema, name) map {
      t =>
        t :: generator.types.values.toList
    }
  }

}