package json.schema.codegen

import json.schema.parser.{JsonSchemaParser, ScalazMatchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Success, Validation}

class ScalaTypeTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers {


  def parse(s: String): Validation[String, ScalaType] = JsonSchemaParser.parse(s).validation.flatMap(ScalaType(_, None))

  ScalaType.getClass.getName should "convert simple types to Scala types" in {
    parse(
      """
        |{"type":"integer"}
      """.stripMargin).map(_.identifier) shouldBe Success("Int")
    parse(
      """
        |{"type":"boolean"}
      """.stripMargin).map(_.identifier) shouldBe Success("Boolean")
    parse(
      """
        |{"type":"number"}
      """.stripMargin).map(_.identifier) shouldBe Success("Double")
    parse(
      """
        |{"type":"integer"}
      """.stripMargin).map(_.identifier) shouldBe Success("Int")
    parse(
      """
        |{"type":"string"}
      """.stripMargin).map(_.identifier) shouldBe Success("String")
  }

  it should "convert array of unique items to Scala Set" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}, "uniqueItems":true
        |}
      """.stripMargin).map(_.identifier) shouldBe Success("Set[String]")
  }

  it should "convert array of items to Scala List" in {
    parse(
      """
        |{"type":"array",
        |"items":{"type":"string"}
        |}
      """.stripMargin).map(_.identifier) shouldBe Success("List[String]")
  }

  it should "use id in camel case for class name" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object"
        |}
      """.stripMargin).map(_.identifier) shouldBe Success("Product")
    parse(
      """
        |{
        | "id": "http://some/path#/product",
        |"type":"object"
        |}
      """.stripMargin).map(_.identifier) shouldBe Success("Product")
  }

  it should "object with properties creates type with members" in {
    parse(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"properties": {
        |"a":{"type":"string"},
        |"b":{"type":"number"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.propertyType))) shouldBe Success(
      List(
        ("a", "String"),
        ("b", "Option[Double]")
      )
    )
  }

  it should "object with properties uses definitions" in {
    parse(
      """
        |{
        | "id": "product",
        |"type":"object",
        |"properties": {
        |"a":{"$ref": "#/definitions/typea"}
        |},
        |"definitions":{
        | "typea":{"type":"string"}
        |},
        |"required":["a"]
        |}
      """.stripMargin).map(_.asInstanceOf[ScalaClass].properties.map(p => (p.name, p.propertyType))) shouldBe Success(
      List(
        ("a", "String")
      )
    )
  }


}
