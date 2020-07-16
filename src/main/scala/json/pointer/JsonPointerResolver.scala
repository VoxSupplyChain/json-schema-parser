package json.pointer

import java.net.URI

import argonaut._

import scala.annotation.tailrec
import scala.util.{Failure, Success}
import scalaz.\/
import scalaz.syntax.either._

/**
  * JsonPointer resolver and decoder for Argonaut Json documents.
  * Based on: http://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-04
  */
object JsonPointerResolver {

  def apply(uriPointer: URI)(json: Json): String \/ Json =
    JsonPointer(uriPointer) match {
      case Success(d) => apply(d)(json)
      case Failure(e) => e.getMessage.left[Json]
    }

  def apply(pointer: JsonPointer)(json: Json): String \/ Json = query(pointer)(json.hcursor)

  @tailrec
  private def query(p: JsonPointer)(a: HCursor): String \/ Json = {
    val value = p.head match {
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
          case None       => c.focus.right
        }
      case None => s"${p.head} not found in ${a.focus}".left
    }
  }

}
