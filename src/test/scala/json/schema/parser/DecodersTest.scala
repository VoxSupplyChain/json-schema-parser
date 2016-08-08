package json.schema.parser

import argonaut.DecodeJson
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Failure, NonEmptyList, Success}

class DecodersTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers with Decoders {

  import argonaut.Argonaut._

  implicit val simpleTypes: Gen[SimpleType.Value] = Gen.oneOf(SimpleType.values.toList)

  implicit val oneOrListOfStrings: DecodeJson[NonEmptyList[String]] = oneOrNonEmptyList[String]

  SimpleType.getClass.toString should "encode and decode" in {
    forAll(simpleTypes) { (c: SimpleType.SimpleType) =>
      c.asJson.nospaces.decodeValidation[SimpleType.SimpleType] shouldBe Success(c)
    }
  }

  "OneOrMoreStrings" should "decode a list into a list" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeValidation[NonEmptyList[String]] shouldBe Success(NonEmptyList("string1", "string2"))
  }

  it should "decode a single item into a list" in {
    """
      |"string1"
    """.stripMargin.decodeValidation[NonEmptyList[String]] shouldBe Success(NonEmptyList("string1"))
  }

  it should "fail on empty list" in {
    """
      |[]
    """.stripMargin.decodeValidation[NonEmptyList[String]] shouldBe a[Failure[_]]
  }

  "SetDecodeJsonStrict" should "decode a valid set into a set" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeValidation[Set[String]](setDecodeJsonStrict) shouldBe Success(Set("string1", "string2"))
  }

  it should "fail decode a invalid set" in {
    """
      |["string1", "string2", "string1"]
    """.stripMargin.decodeValidation[Set[String]](setDecodeJsonStrict) shouldBe Failure("[A]Set[A]: []")
  }

  "NonEmptySetDecodeJsonStrict" should "decode a valid set into a set" in {
    """
      |["string1", "string2"]
    """.stripMargin.decodeValidation[Set[String]](nonEmptySetDecodeJsonStrict) shouldBe Success(Set("string1", "string2"))
  }

  it should "fail decode an empty list" in {
    """
      []
    """.stripMargin.decodeValidation[Set[String]](nonEmptySetDecodeJsonStrict) shouldBe Failure("[A]Set[A]: []")
  }

  "Either" should "decode a number or boolean" in {
    """
      |123
    """.stripMargin.decodeValidation[Either[Long, Boolean]] shouldBe Success(Left(123))
    """
      |true
    """.stripMargin.decodeValidation[Either[Long, Boolean]] shouldBe Success(Right(true))
    """
      |"string"
    """.stripMargin.decodeValidation[Either[Long, Boolean]] shouldBe a[Failure[_]]
  }

}
