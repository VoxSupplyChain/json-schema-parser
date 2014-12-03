package json.schema.parser

import java.net.URI
import java.util.NoSuchElementException

import argonaut.Argonaut._
import argonaut._

import scala.util.matching.Regex


object SchemaDecoders extends DecodeJsons {

  implicit def RegexDecoder: DecodeJson[Regex] = StringDecodeJson.map(_.r)

  implicit def UriDecoder: CodecJson[URI] = CodecJson.derived(
    EncodeJson(v => jString(v.toString)),
    StringDecodeJson.flatMap {
      uri =>
        DecodeJson(
          j => {
            try{
              DecodeResult.ok(new URI(uri))
            } catch {
              case e:NoSuchElementException => DecodeResult.fail("Uri", j.history)
            }
          }
        )
    })

  implicit def formatCodec: CodecJson[Format.Format] = CodecJson[Format.Format]((v: Format.Format) => v.toString.asJson, (j: HCursor) => j.as[String].flatMap {
    s: String =>
      try{
        DecodeResult.ok(Format.withName(s))
      } catch {
        case e:NoSuchElementException => DecodeResult.fail("Format", j.history)
      }
  })

  implicit def simpleTypeCodec: CodecJson[SimpleType.SimpleType] = CodecJson[SimpleType.SimpleType]((v: SimpleType.SimpleType) => v.toString.asJson, (j: HCursor) => j.as[String].flatMap {
    s: String =>
      try{
        DecodeResult.ok(SimpleType.withName(s))
      } catch {
        case e:NoSuchElementException => DecodeResult.fail("SimpleType", j.history)
      }
  })

  implicit def SchemaReferenceJson: CodecJson[SchemaReference] =
    casecodec1(SchemaReference.apply, SchemaReference.unapply)("$ref")

  def NonEmptyListDecodeJson[A](implicit e: DecodeJson[A]): DecodeJson[List[A]] =
    implicitly[DecodeJson[List[A]]] flatMap {
      list =>
        if (list.size > 0) DecodeJson(_ => DecodeResult.ok(list))
        else DecodeJson(c => DecodeResult.fail("[A]List[A]", c.history))
    } setName "[A]List[A]"

  def oneOrNonEmptyList[T](implicit e: DecodeJson[T]): DecodeJson[List[T]] = NonEmptyListDecodeJson[T] ||| e.map(List(_))

  def SetDecodeJsonStrict[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    implicitly[DecodeJson[List[A]]] flatMap {
      list =>
        val set = list.toSet
        if (set.size == list.size) DecodeJson(_ => DecodeResult.ok(set))
        else DecodeJson(c => DecodeResult.fail("[A]List[A]", c.history))
    } setName "[A]Set[A]"

  def NonEmptySetDecodeJsonStrict[A](implicit e: DecodeJson[A]): DecodeJson[Set[A]] =
    SetDecodeJsonStrict[A](e) flatMap {
      set =>
        if (set.size > 0) DecodeJson(_ => DecodeResult.ok(set))
        else DecodeJson(c => DecodeResult.fail("[A]Set[A]", c.history))
    } setName "[A]Set[A]"


  def oneOrSetStrict[T](implicit e: DecodeJson[T]): DecodeJson[Set[T]] = NonEmptySetDecodeJsonStrict[T] ||| e.map(Set(_))

  def either[A, B](x: => DecodeJson[A], y: => DecodeJson[B]): DecodeJson[Either[A, B]] =
    DecodeJson(c => {
      val q = x(c).map(Left(_))
      q.result.fold(_ => y(c).map(Right(_)), _ => q)
    })


}
