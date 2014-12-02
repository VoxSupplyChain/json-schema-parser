package json.schema.parser

import java.io.{FilenameFilter, File}
import java.net.URI
import java.nio.file.{FileStore, Files}

import org.scalacheck.Gen
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FlatSpec, FunSuite}

import scalaz.{Validation, Failure, Success}

trait ScalazMatchers {

  def failureContaining(contain: String) =
    new Matcher[Validation[_, _]] {
      def apply(left: Validation[_, _]) = {
        val r = left match {
          case Failure(e) => e.toString.contains(contain)
          case _ => false
        }
        MatchResult(r, s"$left does not contain '$contain'", s"$left contains '$contain'")
      }
    }

}

class JsonSchemaDecoderFactoryParserTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers{

  JsonSchemaParser.getClass.toString should "parse empty schemas" in {

    JsonSchemaParser(
      """
        |{}
      """.stripMargin) shouldBe a[Success[_, _]]

  }

  it should "parse schemas with optional valid fields" in {

    JsonSchemaParser(
      """
        |{
        |"title":"root"
        |}
      """.stripMargin).map { d: SchemaDocument[Double] => d.title} shouldBe Success(Some("root"))

    JsonSchemaParser(
      """
        |{
        |"title":10
        |}
      """.stripMargin).map { d: SchemaDocument[Double] => d.title} shouldBe Failure("String: [--\\(title)]")

  }

  it should "parse nested schemas" in {

    val result = JsonSchemaParser(
      """
        |{
        |    "title": "root",
        |    "otherSchema": {
        |        "title": "nested",
        |        "anotherSchema": {
        |            "title": "alsoNested"
        |        }
        |    }
        |}
      """.stripMargin)

    result.map(_.title) shouldBe Success(Some("root"))
    result.map(_.nestedSchemas("otherSchema").title) shouldBe Success(Some("nested"))
    result.map(_.nestedSchemas("otherSchema").nestedSchemas("anotherSchema").title) shouldBe Success(Some("alsoNested"))


  }



  it should "parse and validate $schemas" in {

    JsonSchemaParser(
      """
        |{
        |    "$schema":"http://json-schema.org/schema#"
        |}
      """.stripMargin).map(_.schema) shouldBe Success(Some(new URI("http://json-schema.org/schema#")))

    JsonSchemaParser(
      """
        |{
        |    "$schema":"http://json-schema.org/draft-04/schema#"
        |}
      """.stripMargin).map(_.schema) shouldBe Success(Some(new URI("http://json-schema.org/draft-04/schema#")))

    JsonSchemaParser(
      """
        |{
        |    "$schema":"http://json-schema.org/draft-03/schema#"
        |}
      """.stripMargin).map(_.schema) should failureContaining("not supported schema")

  }


  it should "validate id" in {

    JsonSchemaParser(
      """
        |{
        |    "id":"http://x.y.z/rootschema.json#"
        |}
      """.stripMargin).map(_.id) shouldBe Success(new URI("http://x.y.z/rootschema.json#"))

    JsonSchemaParser(
      """
        |{
        |    "id":"#nested"
        |}
      """.stripMargin).map(_.id) shouldBe Success(new URI("#nested"))

    JsonSchemaParser(
      """
        |{
        |    "id":""
        |}
      """.stripMargin).map(_.schema) should failureContaining("not valid id")
    JsonSchemaParser(
      """
        |{
        |    "id":"#"
        |}
      """.stripMargin).map(_.schema) should failureContaining("not valid id")
  }

  it should "resolve id based on parent schema" in {

    val r = JsonSchemaParser(
      """
        |{
        |    "id": "http://x.y.z/rootschema.json#",
        |    "schema1": {
        |        "id": "#foo"
        |    },
        |    "schema2": {
        |        "id": "otherschema.json",
        |        "nested": {
        |            "id": "#bar"
        |        },
        |        "alsonested": {
        |            "id": "t/inner.json#a"
        |        }
        |    },
        |    "schema3": {
        |        "id": "some://where.else/completely#"
        |    }
        |}
      """.stripMargin)

    r.map(_.id) shouldBe Success(new URI("http://x.y.z/rootschema.json#"))
    r.map(_.nestedSchemas("schema1").id) shouldBe Success(new URI("http://x.y.z/rootschema.json#foo"))
    r.map(_.nestedSchemas("schema2").id) shouldBe Success(new URI("http://x.y.z/otherschema.json#"))
    r.map(_.nestedSchemas("schema2").nestedSchemas("nested").id) shouldBe Success(new URI("http://x.y.z/otherschema.json#bar"))
    r.map(_.nestedSchemas("schema2").nestedSchemas("alsonested").id) shouldBe Success(new URI("http://x.y.z/t/inner.json#a"))
    r.map(_.nestedSchemas("schema3").id) shouldBe Success(new URI("some://where.else/completely#"))
  }

  it should "resolve to base schema uri if no id in scope" in {

    val r = JsonSchemaParser(
      """
        |{
        |    "id": "http://x.y.z/rootschema.json#",
        |    "schema4": {
        |
        |    }
        |}
      """.stripMargin)

    r.map(_.id) shouldBe Success(new URI("http://x.y.z/rootschema.json#"))
    r.map(_.nestedSchemas("schema4").id) shouldBe Success(new URI("http://x.y.z/rootschema.json#"))
  }


  it should "resolve agaist empty location if no id" in {

    val r = JsonSchemaParser(
      """
        |{
        |    "$schema":"http://json-schema.org/draft-04/schema#",
        |    "schema4": {
        |            "id": "#bar"
        |    }
        |}
      """.stripMargin)

    r.map(_.id) shouldBe Success(new URI("#"))
    r.map(_.nestedSchemas("schema4").id) shouldBe Success(new URI("#bar"))
  }

//  it should "decodes schema references" in {
//
//    val r = JsonSchemaParser(
//      """
//        |{
//        |    "id": "http://my.site/myschema#",
//        |    "definitions": {
//        |        "schema1": {
//        |            "id": "schema1",
//        |            "type": "integer"
//        |        },
//        |        "schema2", {
//        |            "type": "array",
//        |            "items": { "$ref": "schema1" }
//        |        }
//        |    }
//        |}
//        |""".stripMargin)
//
//    r.map(_.id) shouldBe Success(new URI("http://my.site/myschema#"))
//    r.map(_.definitions("schema1").id) shouldBe Success(new URI("http://my.site/schema1"))
//    r.map(_.definitions("schema2").items) shouldBe Success(new URI("http://my.site/schema1"))
//  }

  implicit val validSchemas: Gen[File] = Gen.oneOf(new File("src/test/resources/json/schema/parser/valid").listFiles(new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name endsWith ".json"
  }).toList)

  it should "parse all valid schemas" in {
    forAll(validSchemas) {
      f: File =>
        val source = scala.io.Source.fromFile(f)
        val lines = source.mkString
        source.close()

        JsonSchemaParser(lines) shouldBe a[Success[_, _]]
    }
  }

}
