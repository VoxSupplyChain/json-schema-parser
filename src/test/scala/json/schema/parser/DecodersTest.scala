package json.schema.parser

import argonaut.DecodeJson
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Failure, NonEmptyList}

class DecodersTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with Decoders {

  import argonaut.Argonaut._

  implicit val simpleTypes: Gen[SimpleType.Value] = Gen.oneOf(SimpleType.values.toList)

  implicit val oneOrListOfStrings: DecodeJson[NonEmptyList[String]] = oneOrNonEmptyList[String]

  SimpleType.getClass.toString should "encode and decode" in {
    forAll(simpleTypes) { (c: SimpleType.SimpleType) =>
      c.asJson.nospaces.decodeEither[SimpleType.SimpleType] shouldBe Right(c)
    }
  }

  "OneOrMoreStrings" should "decode a list into a list" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeEither[NonEmptyList[String]] shouldBe Right(NonEmptyList("string1", "string2"))
  }

  it should "decode a single item into a list" in {
    """
      |"string1"
    """.stripMargin.decodeEither[NonEmptyList[String]] shouldBe Right(NonEmptyList("string1"))
  }

  it should "fail on empty list" in {
    """
      |[]
    """.stripMargin.decodeEither[NonEmptyList[String]] shouldBe a[Left[String, _]]
  }

  "SetDecodeJsonStrict" should "decode a valid set into a set" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeEither[Set[String]](aSetDecodeJsonStrict) shouldBe Right(Set("string1", "string2"))
  }

  it should "fail decode a invalid set" in {
    """
      |["string1", "string2", "string1"]
    """.stripMargin.decodeEither[Set[String]](aSetDecodeJsonStrict) shouldBe Left("[A]Set[A]: CursorHistory(List())")
  }

  "NonEmptySetDecodeJsonStrict" should "decode a valid set into a set" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeEither[Set[String]](nonEmptySetDecodeJsonStrict) shouldBe Right(Set("string1", "string2"))
  }

  it should "fail decode an empty list" in {
    """
      []
    """.stripMargin.decodeEither[Set[String]](nonEmptySetDecodeJsonStrict) shouldBe Left(
      "[A]Set[A]: CursorHistory(List())"
    )
  }

  "Either" should "decode a number or boolean" in {
    """
      |123
    """.stripMargin.decodeEither[Either[Long, Boolean]] shouldBe Right(Left(123))
    """
      |true
    """.stripMargin.decodeEither[Either[Long, Boolean]] shouldBe Right(Right(true))
    """
      |"string"
    """.stripMargin.decodeEither[Either[Long, Boolean]] shouldBe a[Left[String, _]]
  }

}
