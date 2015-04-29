package json.source

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.Json

import scala.collection.mutable
import scala.util.control.NonFatal
import scalaz._


trait JsonSource[A] {

  def uri(addr: A): URI

  def json(addr: A): String \/ Json
}

object JsonSource {

  implicit val json: JsonSource[Json] = new JsonSource[Json] {
    override def uri(t: Json): URI = new URI("#")

    override def json(t: Json): String \/ Json = \/-(t)
  }

  implicit val string: JsonSource[String] = new JsonSource[String] {
    override def uri(t: String): URI = new URI("#")

    override def json(t: String): String \/ Json = t.parse
  }

  implicit val file: JsonSource[File] = new JsonSource[File] {
    override def uri(t: File): URI = t.toURI

    override def json(t: File): String \/ Json = try {
      scala.io.Source.fromFile(t).mkString.parse
    } catch {
      case NonFatal(e) => -\/(e.getMessage)
    }
  }

  implicit val uri: JsonSource[URI] = new JsonSource[URI] {
    override def uri(t: URI): URI = t

    override def json(t: URI): String \/ Json = try {
      import scala.io.Source
      val html = if (t.isAbsolute) Source.fromURL(t.toURL) else Source.fromURI(t)
      val s = html.mkString
      s.parse
    } catch {
      case NonFatal(e) => -\/(e.getMessage)
    }
  }

  def cached[T](implicit wrapped: JsonSource[T]): JsonSource[T] = new JsonSource[T] {

    val cache = mutable.Map.empty[URI, String \/ Json]

    override def uri(t: T): URI = wrapped.uri(t)

    override def json(t: T): \/[String, Json] = {
      // remove fragment, as whole document for that uri is cached
      val key = uri(t).resolve("#")
      cache.getOrElseUpdate(key, wrapped.json(t))
    }
  }
}
