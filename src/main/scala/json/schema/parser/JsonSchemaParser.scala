package json.schema.parser

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.{DecodeJson, Json}
import json.reference.ReferenceResolver
import json.schema.scope.{ExpandReferences, ScopeDiscovery}

import scalaz.Scalaz._
import scalaz._

trait JsonSource[T] {

  def uri(t: T): URI

  def json(t: T): String \/ Json
}

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

    override def json(t: URI): String \/ Json = scala.io.Source.fromURI(t).mkString.parse
  }

}

class JsonSchemaParser[N](implicit n: Numeric[N], dn: DecodeJson[N]) {

  def schemaDecoder(uri: URI) = JsonSchemaDecoderFactory[N](uri)

  def read[T: JsonSource](source: T): String \/ Json = implicitly[JsonSource[T]].json(source).flatMap {
    json =>

      val rootUri: URI = implicitly[JsonSource[T]].uri(source)
      for {
        expandedJson <- ExpandReferences.expand(rootUri, json.hcursor)
        idMap <- ScopeDiscovery.scopes(rootUri, expandedJson.hcursor)
        local: ReferenceResolver = new ReferenceResolver(defaultLoader = {
          reference: URI =>
            idMap.get(reference).fold[String \/ Json](-\/(s"no scope $reference"))(j => \/-(j)) orElse ReferenceResolver.fromURI(reference)
        }.some)
        resolved <- local.resolvePointer(rootUri)(expandedJson, rootUri)
      } yield resolved
  }


  private def parseToSchema(uri: URI)(j: Json) = j.jdecode(schemaDecoder(uri)).toDisjunction.leftMap(r => r._1 + ": " + r._2.shows)

  def parse[T: JsonSource](source: T): String \/ SchemaDocument[N] = read(source).flatMap(parseToSchema(implicitly[JsonSource[T]].uri(source)))

}

object JsonSchemaParser extends JsonSchemaParser[Double]