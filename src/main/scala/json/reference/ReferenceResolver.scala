package json.reference

import java.net.URI

import argonaut.Json
import json.pointer.JsonPointerDecodeJson
import json.source.JsonSource

import scala.collection.immutable.Stack
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 */
class ReferenceResolver(defaultLoader: Loader) {

  def relative(parent: URI, sub: URI) = {
    val resolved = parent.resolve(sub)
    if (resolved.getFragment == null || resolved.getFragment.isEmpty) resolved.resolve("#") else resolved
  }


  private def resolve(json: Json, root: Json, rootURI: URI, inprogress: Stack[URI]): String \/ Json = new ReferenceTraverser {

    override def resolve: (URI) => \/[String, Json] = {
      uri =>

        val resolved = if (uri.toString.startsWith("#"))
          resolvePointer(uri, root, rootURI, inprogress)
        else {
          val expandedURI: URI = relative(rootURI, uri)
          resolveReference(expandedURI, rootURI, defaultLoader, inprogress)
        }

        resolved leftMap (cause => s"reference $uri not found: $cause")
    }

  }.traverse(json.hcursor)

  def resolvePointer(reference: URI, root: Json, rootURI: URI, inprogress: Stack[URI]): String \/ Json = {
    resolveReference(reference, rootURI, uri => \/-((root, uri)), inprogress)
  }

  /**
   * @param reference reference with a pointer
   * @return
   */
  def resolveReference(reference: URI, rootURI: URI, loader: Loader, inprogress: Stack[URI]): String \/ Json = {
    if (inprogress.contains(reference))
      -\/(s"found cyclic reference: $reference")
    else {
      // the refered Json document must be resolved as well
      loader(reference).flatMap {
        case (root, updatedReference) =>
          JsonPointerDecodeJson(updatedReference).flatMap(d => d(root.hcursor).toDisjunction.leftMap(_.toString())) flatMap {
            pointedNode =>
              resolve(pointedNode, root, rootURI, inprogress.push(reference))
          }

      }
    }
  }

}


object ReferenceResolver {


  val local: ReferenceResolver = new ReferenceResolver(defaultLoader = {
    uri: URI =>
      JsonSource.uri.json(uri).map((_, uri))
  })

  def apply[T](addr: T)(implicit src: JsonSource[T]): String \/ Json =
    src.json(addr).flatMap(root => {
      val uri = src.uri(addr)
      local.resolvePointer(uri, root, uri, Stack.empty)
    })

}