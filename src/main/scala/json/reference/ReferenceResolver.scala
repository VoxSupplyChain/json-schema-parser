package json.reference

import java.net.URI

import argonaut.Json
import json.pointer.JsonPointerDecodeJson
import json.source.JsonSource

import scala.collection.immutable.Stack
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 * @param inprogress current resolution stack, to help tracking cyclic dependencies
 */
case class ReferenceResolver(inprogress: Stack[URI] = Stack(new URI("")), defaultLoader: Loader ) {

  def relative(parent: URI, sub: URI) = {
    val resolved = parent.resolve(sub)
    if (resolved.getFragment == null || resolved.getFragment.isEmpty) resolved.resolve("#") else resolved
  }


  private def resolve(json: Json)(implicit root: Json, rootURI: URI): String \/ Json = new ReferenceTraverser {

    override def resolve: (URI) => \/[String, Json] = {
      uri =>

        val resolved = if (uri.toString.startsWith("#"))
          resolvePointer(uri)(root, rootURI)
        else
          resolveReference(relative(rootURI, uri))(rootURI, defaultLoader)

        resolved leftMap (cause => s"reference $uri not found: $cause")
    }

  }.traverse(json.hcursor)

  def resolvePointer(reference: URI)(implicit root: Json, rootURI: URI): String \/ Json = {
    resolveReference(reference)(rootURI, uri => \/-((root, uri)))
  }

  /**
   * @param reference reference with a pointer
   * @return
   */
  def resolveReference(reference: URI)(implicit rootURI: URI, loader: Loader): String \/ Json = {
    if (inprogress.contains(reference))
      -\/(s"found cyclic reference: $reference")
    else {
      // the refered Json document must be resolved as well
      loader(reference).flatMap {
        case (root, updatedReference) =>
          JsonPointerDecodeJson(updatedReference).flatMap(d => d(root.hcursor).toDisjunction.leftMap(_.toString())) flatMap {
            pointedNode =>
              val nestedResolver = this.copy(inprogress.push(reference))
              nestedResolver.resolve(pointedNode)(root, rootURI)
          }

      }
    }
  }

}


object ReferenceResolver {


  val local: ReferenceResolver = new ReferenceResolver(defaultLoader = {
    uri: URI =>
      JsonSource.uri.json(uri).map( (_, uri))
  })

  def apply[T](addr: T)(implicit src:JsonSource[T]): String \/ Json =
    src.json(addr).flatMap(root => {
      val uri = src.uri(addr)
      local.resolvePointer(uri)(root, uri)
    })

}