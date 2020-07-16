package json.schema.scope

import java.net.URI

import argonaut.Argonaut._
import argonaut.Json
import json.schema.parser.ScalazMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.Validation
import scalaz.syntax.std.either._

class ExpandReferencesTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers {

  def expand(s: String): Validation[String, Json] =
    s.parse.disjunction
      .flatMap(r => ExpandReferences.expand(new URI("http://x.y.z/rootschema.json#"), r))
      .validation

  ExpandReferences.getClass.getName should "expand references to absolute based on parent scopes" in {

    expand("""
              |{
              |    "id": "http://x.y.z/rootschema.json#",
              |    "schema1": {
              |        "id": "#foo",
              |        "schema1.1": {
              |        "$ref": "#anotherfoo"
              |        }
              |    },
              |    "schema2": {
              |     "$ref" : "anotherroot.json#"
              |    }
              |}
               """.stripMargin) shouldBe """
                                |{
                                |    "id": "http://x.y.z/rootschema.json#",
                                |    "schema1": {
                                |        "id": "http://x.y.z/rootschema.json#foo",
                                |        "schema1.1": {
                                |        "$ref": "http://x.y.z/rootschema.json#anotherfoo"
                                |        }
                                |    },
                                |    "schema2": {
                                |     "$ref" : "http://x.y.z/anotherroot.json#"
                                |    }
                                |}
                                | """.stripMargin.parse.validation
  }

  it should "expand references with file's scope if no id" in {

    expand("""
              |{
              |    "schema1": {
              |        "id": "#foo",
              |        "schema1.1": {
              |        "$ref": "#anotherfoo"
              |        }
              |    },
              |    "schema2": {
              |     "$ref" : "anotherroot.json#"
              |    }
              |}
            """.stripMargin) shouldBe """
                                        |{
                                        |    "schema1": {
                                        |        "id": "http://x.y.z/rootschema.json#foo",
                                        |        "schema1.1": {
                                        |        "$ref": "http://x.y.z/rootschema.json#anotherfoo"
                                        |        }
                                        |    },
                                        |    "schema2": {
                                        |     "$ref" : "http://x.y.z/anotherroot.json#"
                                        |    }
                                        |}
                                        | """.stripMargin.parse.validation

  }

  it should "expand references in nested scopes" in {

    expand("""
              |{
              |    "schema1": {
              |        "id": "foo",
              |        "schema1.1": {
              |        "$ref": "#anotherfoo"
              |        }
              |    }
              |}
            """.stripMargin) shouldBe """
                                        |{
                                        |    "schema1": {
                                        |        "id": "http://x.y.z/foo#",
                                        |        "schema1.1": {
                                        |        "$ref": "http://x.y.z/foo#anotherfoo"
                                        |        }
                                        |    }
                                        |}
                                        | """.stripMargin.parse.validation

  }

}
