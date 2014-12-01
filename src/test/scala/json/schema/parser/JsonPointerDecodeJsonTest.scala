package json.schema.parser

import argonaut.Argonaut._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Failure, Success}

class JsonPointerDecodeJsonTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with ScalazMatchers {

  val json =
    """
      |{
      | "a": {
      |   "b": 1,
      |   "c": [1,2,3,4]
      | }
      |}
    """.stripMargin.parse.validation.getOrElse(throw new IllegalArgumentException)

  JsonPointerDecodeJson.getClass.toString should " / points to root " in {
    JsonPointerDecodeJson(JsonPointer("").get)(json.hcursor).toDisjunction.validation shouldBe Success(json)
  }

  it should " /<key> points to a node " in {
    JsonPointerDecodeJson(JsonPointer("/a/b").get)(json.hcursor).toDisjunction.validation shouldBe Success(jNumber(1))
  }

  it should " /<key>/<number> points to an array " in {
    JsonPointerDecodeJson(JsonPointer("/a/c/2").get)(json.hcursor).toDisjunction.validation shouldBe Success(jNumber(3))
  }

  it should " fail for /<unknown> " in {
    JsonPointerDecodeJson(JsonPointer("/f").get)(json.hcursor).toDisjunction.validation should failureContaining("f not found")
  }

  it should " fail for array index out of bounds /a/c/<unknown> " in {
    JsonPointerDecodeJson(JsonPointer("/a/c/10").get)(json.hcursor).toDisjunction.validation should failureContaining("10 not found")
  }

}
