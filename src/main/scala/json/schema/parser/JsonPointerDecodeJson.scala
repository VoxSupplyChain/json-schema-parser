package json.schema.parser

import java.net.URI

import argonaut._

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scalaz.Validation
import scalaz.Scalaz._

/**
 * JsonPointer resolver and decoder for Argonaut Json documents.
 * Based on: http://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-04
 */
object JsonPointerDecodeJson {

  def apply(u: URI): Validation[String, DecodeJson[Json]] = JsonPointer(u.getFragment).map(p=>DecodeJson(query(p))) match {
    case Success(d) => scalaz.Success(d)
    case Failure(e) => scalaz.Failure(e.getMessage)
  }

  def apply(p: JsonPointer): DecodeJson[Json] = DecodeJson(query(p))

  @tailrec
  def query(p: JsonPointer)(a: HCursor): DecodeResult[Json] = {
    val value= p.head match {
      case JsonPointerRootStep =>
        Some(a)
      case JsonPointerStringStep(key) =>
        a.downField(key).hcursor
      case JsonPointerNumericStep(key) =>
        a.downN(key).hcursor
    }
    value match {
      case Some(c) =>
        p.tail match {
          case Some(next) => query(next)(c)
          case None => DecodeResult.ok(c.focus)
        }
      case None => DecodeResult.fail(s"${p.head} not found in ${a.focus}", a.history)
    }
  }

}
