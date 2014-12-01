package json.schema.parser

import argonaut._

import scala.annotation.tailrec

object JsonPointerDecodeJson {

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
