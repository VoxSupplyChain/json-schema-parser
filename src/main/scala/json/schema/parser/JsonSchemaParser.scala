package json.schema.parser

import java.net.URI

import argonaut.Argonaut._
import argonaut.{DecodeJson, Json}
import json.reference.ReferenceResolver
import json.schema.scope.{ExpandReferences, ScopeDiscovery}

import scalaz.Scalaz._
import scalaz._

class JsonSchemaParser[N](implicit n: Numeric[N], dn: DecodeJson[N]) {

  def schemaDecoder(uri: URI) = JsonSchemaDecoderFactory[N](uri)

  def read[T: JsonSource](addr: T)(implicit source: JsonSource[T]): String \/ Json = source.json(addr).flatMap {
    json =>

      val cachingUriSource = JsonSource.cachedSource(implicitly[JsonSource[URI]])

      val rootUri: URI = source.uri(addr)
      for {
        expandedJson <- ExpandReferences.expand(rootUri, json.hcursor)
        idMap <- ScopeDiscovery.scopes(rootUri, expandedJson.hcursor)
        local: ReferenceResolver = new ReferenceResolver(defaultLoader = {
          reference: URI =>
            idMap.get(reference).fold[String \/ Json](-\/(s"no scope $reference"))(j => \/-(j)) orElse cachingUriSource.json(reference)
        }.some)
        resolved <- local.resolvePointer(rootUri)(expandedJson, rootUri)
      } yield resolved
  }


  private def parseToSchema(uri: URI)(j: Json) = j.jdecode(schemaDecoder(uri)).toDisjunction.leftMap(r => r._1 + ": " + r._2.shows)

  def parse[T: JsonSource](source: T): String \/ SchemaDocument[N] = read(source).flatMap(parseToSchema(implicitly[JsonSource[T]].uri(source)))

}

object JsonSchemaParser extends JsonSchemaParser[Double]