package json.schema.codegen

import json.schema.parser.{JsonSchemaParser, ScalazMatchers}
import org.scalactic.{Explicitly, StringNormalizations}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec, FunSuite}

import scalaz.{Success, Validation}

class CodeGenTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers with CodeGen {


  def parse(s: String): Validation[String, Set[ScalaType]] = JsonSchemaParser.parse(s).validation.flatMap(ScalaModelGenerator(_))

  def gen(s: String): Validation[String, String] = parse(s) map {
    ts => ts.map(genType).mkString("\n").trim
  }

  "CodeGen" should "generate type with optional properties" in {
    gen( """
           |{
           | "id": "http://some/product",
           |"type":"object",
           |"properties": {
           |"a":{"type":"string"},
           |"b":{"type":"number"}
           |},
           |"required":["a"]
           |}
         """.stripMargin) shouldBe Success( """case class Product(a:String, b:Option[Double])""".stripMargin.trim)
  }

  it should "generate type with array properties" in {
    gen( """
           |{
           | "id": "http://some/product",
           |"type":"object",
           |"properties": {
           |"a":{"type":"array", "items":{"type":"string"}},
           |"b":{"type":"array", "items":{"type":"number"}}
           |},
           |"required":["a"]
           |}
         """.stripMargin) shouldBe Success( """case class Product(a:List[String], b:Option[List[Double]])""".stripMargin.trim)
  }

  it should "generate type with nested types" in {
    gen( """
           |{
           | "id": "http://some/product",
           |"type":"object",
           |"properties": {
           |"a":{"type":"array", "items":{"$ref":"#/definitions/nested"}},
           |"b":{"type":"array", "items":{"type":"number"}}
           |},
           |"required":["a"],
           |"definitions": {
           |"nested": {
           |"id":"#/definitions/nested",
           |"type":"object"
           | }
           |}
           |
           |}
         """.stripMargin) shouldBe Success( """
                                              |case class Product(a:List[Nested], b:Option[List[Double]])
                                              |case class Nested()
                                              | """.stripMargin.trim)
  }

  it should "generate enumeration with values " in {
    gen( """
           |{
           | "id": "http://some/product",
           |"type":"string",
           |"enum":["a 1","b"]
           |}
         """.stripMargin) shouldBe Success( """
                                              |object Product extends Enumeration {
                                              |val a_1 = Value("a 1")
                                              |val b = Value("b")
                                              |}""".stripMargin.trim)
    gen( """
           |{
           | "id": "http://some/product",
           |"type":"integer",
           |"enum":[1,2]
           |}
         """.stripMargin).map(_.replaceAll("\\s", "")) shouldBe Success( """
                                                                           |object Product extends Enumeration {
                                                                           |val v1 = Value(1)
                                                                           |val v2 = Value(2)
                                                                           |}""".stripMargin.trim.replaceAll("\\s", ""))
  }


  it should "generate type with additional properties in a map" in {
    gen(
      """
        |{
        | "id": "http://some/product",
        |"type":"object",
        |"additionalProperties":{"$ref":"#/definitions/nested"},
        |"definitions": {
        |"nested": {
        |"id":"#/definitions/nested",
        |"type":"object"
        | }
        |}
        |
        |}
      """.stripMargin) shouldBe
      Success(
        """
          |case class Product(_additional:Map[String, Nested])
          |case class Nested()
          | """.stripMargin.trim)
  }

}
