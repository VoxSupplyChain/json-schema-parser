package json.reference

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz._

class ReferenceTraverserTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {


  import argonaut.Argonaut._
  import argonaut._

  def replaceRefWithNull(json: Json): ReferenceTraverser.TraverseResult = ReferenceTraverser(json.hcursor)(_ => jNull)

  def shouldNotChange(json: String) = {
    val j = json.stripMargin.parseOption.get
    replaceRefWithNull(j) shouldBe Success(j)
  }

  def shouldChange(json: String, to: String) = {
    val j = json.stripMargin.parseOption.get
    val t = to.stripMargin.parseOption.get
    replaceRefWithNull(j) shouldBe Success(t)
  }

  def shouldFail(json: String) = {
    val j = json.stripMargin.parseOption.get
    replaceRefWithNull(j) shouldBe a[Failure[_,_]]
  }

  "JsonTraverser" should "not modify json without references" in {

    shouldNotChange(
      """
        |1
        | """
    )

    shouldNotChange(
      """
        |null
        | """
    )

    shouldNotChange(
      """
        |{
        | "a": 1,
        | "b": 2,
        | "c": 3
        |}
        | """
    )

    shouldNotChange(
      """
        |[1,2,3,4]
      """
    )

    shouldNotChange(
      """
        |[1,{ "a":2},3,4]
      """
    )


  }
  "JsonTraverser" should " modify json references" in {

    shouldChange(
      """
        |{
        | "$ref": "#uri"
        |}
      """,
      """
        null
      """
    )

    shouldChange(
      """
        |{
        | "a": {"$ref":"#/path"},
        | "b": [{"$ref":"#/path"},{ "$ref": "#uri" },{"$ref":"#/path"},{"$ref":"#/path"}],
        | "c": [ { "x": {"$ref":"#/path"} } ],
        | "d": {"$ref":"#/path"}
        |}
      """,
      """
        |{
        | "a": null,
        | "b": [null,null,null,null],
        | "c": [ { "x":null}],
        | "d":null
        |}
      """
    )

    shouldChange(
      """
        |[{
        | "$ref": "#uri"
        |}]
      """,
      """
        [null]
      """
    )
  }


  "JsonTraverser" should " fail for invalid references" in {

    shouldFail(
      """
        |{
        | "$ref": " b e a failure"
        |}
      """)
    shouldFail(
      """
        |[{
        | "$ref": "- url"
        |}]
      """)
  }
}
