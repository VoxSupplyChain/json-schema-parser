package json.reference

import java.net.URI

import argonaut.Json
import json.pointer.{JsonPointer, JsonPointerResolver}
import json.source.JsonSource

import scala.collection.immutable.Stack
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 */
trait ReferenceResolver {
  type Loader = URI => String \/ (Json, URI)

  // for faster resolution of already referenced resources
  private val cachingUriSource: JsonSource[URI] = JsonSource.withCaching[URI]

  protected def defaultLoader: Loader = {
    uri: URI =>
      cachingUriSource.json(uri).map((_, uri))
  }

  protected def dereference(reference: URI, rootURI: URI, loader: Loader, inprogress: Stack[URI]): String \/ Json =
    if (inprogress.contains(reference))
      \/-(Json("$ref" -> Json.jString(reference.toString)))
    else {
      // the refered Json document must be resolved as well
      loader(reference).flatMap {
        case (root, updatedReference) =>
          JsonPointerResolver(updatedReference)(root) flatMap {
            pointedNode =>
              val inprogressNew: Stack[URI] = inprogress.push(reference)
              new ReferenceTraverser {
                override def resolve(ref: URI): String \/ Json = {
                  val resolved = if (ref.toString.startsWith("#"))
                    dereferenceInline(ref, root, rootURI, inprogressNew)
                  else {
                    val expandedURI: URI = JsonPointer.resolveAsPointer(rootURI, ref)
                    dereference(expandedURI, rootURI, defaultLoader, inprogressNew)
                  }

                  resolved leftMap (cause => s"reference $ref not found: $cause")
                }

              }.traverse(pointedNode.hcursor)
          }

      }
    }

  def dereferenceInline(pointer: URI, root: Json, rootURI: URI, inprogress: Stack[URI]): String \/ Json = {
    dereference(pointer, rootURI, uri => \/-((root, uri)), inprogress)
  }

}

object ReferenceResolver {

  def resolveFrom[T](addr: T)(implicit src: JsonSource[T]): String \/ Json =
    src
      .json(addr)
      .flatMap(root => {
        val local: ReferenceResolver = new ReferenceResolver {}
        val uri = src.uri(addr)
        local.dereferenceInline(uri, root, uri, Stack.empty)
      })

}
