package json.schema.parser

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.Json

import scala.collection.mutable
import scala.util.control.NonFatal
import scalaz._

object JsonSource {

  implicit val jsonRef: JsonSource[Json] = new JsonSource[Json] {
    override def uri(t: Json): URI = new URI("#")

    override def json(t: Json): String \/ Json = \/-(t)
  }

  implicit val stringRef: JsonSource[String] = new JsonSource[String] {
    override def uri(t: String): URI = new URI("#")

    override def json(t: String): String \/ Json = t.parse
  }

  implicit val fileRef: JsonSource[File] = new JsonSource[File] {
    override def uri(t: File): URI = t.toURI

    override def json(t: File): String \/ Json = scala.io.Source.fromFile(t).mkString.parse
  }

  implicit val uriRef: JsonSource[URI] = new JsonSource[URI] {
    override def uri(t: URI): URI = t

    override def json(t: URI): String \/ Json = try {
      scala.io.Source.fromURI(t).mkString.parse
    } catch {
      case NonFatal(e) => scala.io.Source.fromURL(t.toURL).mkString.parse
    }
  }

  def cachedSource[T](implicit wrapped: JsonSource[T]) = new JsonSource[T] {

    val cache = mutable.Map.empty[URI, String \/ Json]

    override def uri(t: T): URI = wrapped.uri(t)

    override def json(t: T): \/[String, Json] = {
      // remove fragment, as whole document for that uri is cached
      val key = uri(t).resolve("#")
      cache.getOrElseUpdate(key, wrapped.json(t))
    }
  }
}

trait JsonSource[T] {

  def uri(t: T): URI

  def json(t: T): String \/ Json
}