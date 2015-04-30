package json.pointer

import java.net.URI

import argonaut._

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scalaz._

/**
 * JsonPointer resolver and decoder for Argonaut Json documents.
 * Based on: http://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-04
 */
object JsonPointerDecodeJson {

  def apply(u: URI): String \/ DecodeJson[Json] = {
    val pointer = Option(u.getFragment).map(JsonPointer.apply).getOrElse(Success(JsonPointer.root))
    pointer.map(p => DecodeJson(query(p))) match {
      case Success(d) => \/-(d)
      case Failure(e) => -\/(e.getMessage)
    }
  }

  def apply(p: JsonPointer): DecodeJson[Json] = DecodeJson(query(p))

  @tailrec
  private def query(p: JsonPointer)(a: HCursor): DecodeResult[Json] = {
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
