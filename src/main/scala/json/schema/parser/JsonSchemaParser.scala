package json.schema.parser

import java.net.URI

import argonaut.Argonaut._
import argonaut.{DecodeJson, Json}
import json.reference.{Loader, ReferenceResolver}
import json.schema.scope.{ExpandReferences, ScopeDiscovery}
import json.source.JsonSource

import scala.collection.immutable.Stack
import scalaz.Scalaz._
import scalaz._

class JsonSchemaParser[N](implicit n: Numeric[N], dn: DecodeJson[N]) {

  def schemaDecoder(uri: URI) = JsonSchemaDecoderFactory[N](uri)

  def read[T: JsonSource](addr: T)(implicit source: JsonSource[T]): String \/ Json = source.json(addr).flatMap {
    json =>

      val cachingUriSource = JsonSource.cached(implicitly[JsonSource[URI]])

      val rootUri: URI = source.uri(addr)
      for {
        expandedJson <- ExpandReferences.expand(rootUri, json.hcursor)
        idMap <- ScopeDiscovery.scopes(rootUri, expandedJson.hcursor)
        local: ReferenceResolver = new ReferenceResolver(defaultLoader = {
          reference: URI =>
            val referenceRootDoc = reference.resolve("#")
            idMap.get(reference).map((_, referenceRootDoc)).orElse(idMap.get(referenceRootDoc).map((_, reference)))
              .fold[String \/ (Json, URI)](-\/(s"no scope $reference"))(j => \/-(j)) orElse cachingUriSource.json(reference).map((_, reference))

        }) {

          override def resolveReference(reference: URI)(implicit rootURI: URI, loader: Loader, inprogress: Stack[URI]): \/[String, Json] = {
            // preserve the reference used for loading the json in the *id* field, so it is know where the node came from.
            super.resolveReference(reference).map(result => jsonWithId(result, reference))
          }

        }
        resolved <- local.resolvePointer(rootUri)(expandedJson, rootUri, Stack.empty)
      } yield resolved
  }

  private def jsonWithId(json: Json, id: URI): Json = json.withObject {
    j =>
      val updatedJ = if (j.fields.contains("id"))
        j
      else
        j +("id", jString(id.toString))
      updatedJ
  }

  private def parseToSchema(uri: URI)(j: Json) = j.jdecode(schemaDecoder(uri)).toDisjunction.leftMap(r => r._1 + ": " + r._2.shows)

  def parse[T: JsonSource](source: T): String \/ SchemaDocument[N] = read(source).flatMap(parseToSchema(implicitly[JsonSource[T]].uri(source)))

}

object JsonSchemaParser extends JsonSchemaParser[Double]