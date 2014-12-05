package json.schema.parser

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.{DecodeJson, Json}
import json.reference.ReferenceResolver

import scalaz.Scalaz._
import scalaz._


class JsonSchemaParser[N](implicit n: Numeric[N], dn: DecodeJson[N]) {

  def schemaDecoder(uri: URI) = JsonSchemaDecoderFactory[N](uri)

  private def parseToSchema(uri: URI)( j: Json) = j.jdecode(schemaDecoder(uri)).toDisjunction.leftMap(r => r._1 +": "+r._2.shows)

  def parse(file: File): String \/ SchemaDocument[N] = ReferenceResolver(file).flatMap(parseToSchema(file.toURI))

  def parse(uri: URI): String \/ SchemaDocument[N] = ReferenceResolver(uri).flatMap(parseToSchema(uri))

  def parse(json: Json): String \/ SchemaDocument[N] = ReferenceResolver(json).flatMap(parseToSchema(new URI("#")))

}

object JsonSchemaParser extends JsonSchemaParser[Double] {

  def apply(json: String): Validation[String, SchemaDocument[Double]] = json.parse.flatMap(parse).validation
}