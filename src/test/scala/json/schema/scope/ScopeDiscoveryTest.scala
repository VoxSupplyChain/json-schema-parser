package json.schema.scope

import java.net.URI

import argonaut.Argonaut._
import json.schema.parser.ScalazMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.Success

class ScopeDiscoveryTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers {


  val r =
    """
      |{
      |    "id": "http://my.site/myschema#",
      |    "definitions": {
      |        "schema1": {
      |            "id": "schema1",
      |            "type": "integer"
      |        },
      |        "schema2": {
      |            "type": "array",
      |            "items": { "$ref": "schema1" }
      |        },
      |        "schema2.1": {
      |        "schema3": {
      |            "id": "#pointeroverride",
      |            "type": "number"
      |        }
      |        }
      |    }
      |}
      | """.stripMargin.parseWith(j => j, e => throw new IllegalArgumentException("Json invalid" + e))

  val map = ScopeDiscovery.scopes(new URI("http://myuri"), r.hcursor).validation

  ScopeDiscovery.getClass.getName should "override root scope" in {

    map.map(_(new URI("http://my.site/myschema#"))) shouldBe Success(r)
  }

  it should "map resolve sub scopes based on the parent scopes" in {

    map.map(_(new URI("http://my.site/schema1#"))) shouldBe Success(
      """
        |{
        |            "id": "schema1",
        |            "type": "integer"
        |}
      """.stripMargin.parseOption.get)

    map.map(_(new URI("http://my.site/schema1#pointeroverride"))) shouldBe Success(
      """
        |{
        |            "id": "#pointeroverride",
        |            "type": "number"
        |}""".stripMargin.parseOption.get)

  }

  it should " use document root uri if root id not provided" in {

    val r =
      """
        |{
        |    "definitions": {
        |        "schema1": {
        |            "id": "schema1",
        |            "type": "integer"
        |        }
        |    }
        |}""".stripMargin.parseWith(j => j, e => throw new IllegalArgumentException("Json invalid" + e))

    val map = ScopeDiscovery.scopes(new URI("http://myuri"), r.hcursor).validation

    map.map(_(new URI("http://myuri"))) shouldBe Success(r)
  }

}
