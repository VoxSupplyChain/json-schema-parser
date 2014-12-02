package json.reference

import argonaut.Argonaut._
import json.schema.parser.ScalazMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._

class ReferenceResolverTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers {


  def shouldResolve(from: String, to: String) = from.stripMargin.parse.flatMap(j => ReferenceResolver(j)) shouldBe to.stripMargin.parse

  def shouldFailResolve(from: String) = from.stripMargin.parse.flatMap(j => ReferenceResolver(j)) shouldBe a [-\/[_]]


  ReferenceResolver.getClass.toString should "not change json doc if no references" in {
    shouldResolve(
      """
        |{
        | "source": "sourcedata",
        | "target": "source"
        |}
      """,
      """
        |{
        | "source": "sourcedata",
        | "target": "source"
        |}
      """
    )
  }

  it should "resolve fragment references (http://json-schema.org/address#/dependencies/extended-address) from remove doc" in {
    shouldResolve(
      """
        |{
        | "$ref": "http://json-schema.org/address#/dependencies/extended-address"
        |}
      """,
      """
        |"street-address"
      """
    )
  }

  it should "resolve fragment references (#/source) from current doc" in {
    shouldResolve(
      """
        |{
        | "source": "sourcedata",
        | "$ref": "#/source"
        |}
      """,
      """
        |"sourcedata"
      """
    )
  }

  it should "resolve fragment references (#/source) from nested nodes doc" in {
    shouldResolve(
      """
        |{
        | "source": "sourcedata",
        | "target": {"$ref": "#/source"}
        |}
      """,
      """
        | {
        | "source": "sourcedata",
        | "target": "sourcedata"
        | }
      """
    )
  }

  it should "resolve fragment references (#/source) from remote doc" in {
    shouldResolve(
      """
        |{
        | "source": {"$ref": "http://json-schema.org/address#/type"},
        | "target": [{"$ref": "http://json-schema.org/address#/dependencies/extended-address"}, {"$ref": "http://json-schema.org/address#/required"}],
        | "target2": {"$ref": "#/target"}
        |}
      """,
      """
        | {
        | "source": "object",
        | "target": ["street-address", ["locality", "region", "country-name"]],
        | "target2": ["street-address", ["locality", "region", "country-name"]]
        | }
      """
    )
  }


  it should "fail to resolve unknown references" in {
    shouldFailResolve(
      """
        |{
        | "a": {"$ref": "#/b"}
        |}
      """
    )
  }

  it should "fail to resolve cyclic references" in {
    shouldFailResolve(
      """
        |{
        | "a": {"$ref": "#/b"},
        | "b": {"$ref": "#/c"},
        | "c": {"$ref": "#/a"}
        |}
      """
    )
  }

}
