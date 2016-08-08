package json.schema.parser

import java.net.URI

import argonaut.Argonaut._
import argonaut.{DecodeJson, Json}
import json.reference.ReferenceResolver
import json.schema.scope.{ScopeDiscovery, ExpandReferences}
import json.source.JsonSource

import scala.collection.immutable.Stack
import scalaz._
import scalaz.std.string._
import scalaz.syntax.show._

/**
 * Resolves references using predefined map of scopes first and then using standard resolution.
 * @param resolutionScope
 */
class ScopeReferenceResolver(resolutionScope: Map[URI, Json]) extends ReferenceResolver {

  override protected val defaultLoader: Loader = {
    reference: URI =>
      val referenceRootDoc = reference.resolve("#")
      for {
        (resultJson, resultRef) <- {
          // try to resolve from ID scopes first
          val scopedResult = resolutionScope
            .get(reference)
            .map((_, referenceRootDoc))
            .orElse(resolutionScope.get(referenceRootDoc).map((_, reference)))
            .fold[String \/ (Json, URI)](-\/(s"no scope $reference"))(j => \/-(j))

          scopedResult orElse super.defaultLoader(reference)
        }
        expandedResult <- ExpandReferences.expand(resultRef, resultJson)
      } yield (expandedResult, resultRef)
  }

  override def dereference(reference: URI, rootURI: URI, loader: Loader, inprogress: Stack[URI]): \/[String, Json] = {
    // preserve the reference used for loading the json in the *id* field, so it is know where the node came from.
    super.dereference(reference, rootURI, loader, inprogress).map(result => jsonWithId(result, reference))
  }

  private def jsonWithId(json: Json, id: URI): Json = json.withObject {
    j =>
      if (j.fields.contains("id"))
        j
      else
        j +("id", jString(id.toString))
  }

}

class JsonSchemaParser[N](implicit n: Numeric[N], dn: DecodeJson[N]) {

  def read[T: JsonSource](addr: T)(implicit source: JsonSource[T]): String \/ Json = source.json(addr).flatMap {
    json =>
      val rootUri: URI = source.uri(addr)
      for {
        expandedJson <- ExpandReferences.expand(rootUri, json)
        scopeMap <- ScopeDiscovery.scopes(rootUri, expandedJson)
        local: ReferenceResolver = new ScopeReferenceResolver(scopeMap)
        resolved <- local.dereferenceInline(rootUri, expandedJson, rootUri, Stack.empty)
      } yield resolved
  }

  def parse[T: JsonSource](addr: T): String \/ SchemaDocument[N] =
    read(addr)
      .flatMap(parseToSchema(addr))

  private def parseToSchema[T: JsonSource](addr: T)(json: Json) =
    json
      .jdecode(schemaDecoder(addr))
      .toDisjunction
      .leftMap(r => r._1 + ": " + r._2.shows)

  private def schemaDecoder[T: JsonSource](addr: T)(implicit src: JsonSource[T]) =
    JsonSchemaDecoder[N](parentId = src.uri(addr))

}

object JsonSchemaParser extends JsonSchemaParser[Double]
