package json.reference

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.Json
import json.pointer.JsonPointerDecodeJson
import json.source.JsonSource

import scala.collection.immutable.Stack
import scala.io.Source
import scala.util.control.NonFatal
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 * @param inprogress current resolution stack, to help tracking cyclic dependencies
 */
case class ReferenceResolver(inprogress: Stack[URI] = Stack(new URI("")), defaultLoader: URI => String \/ Json ) {

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
    resolveReference(reference)(rootURI, _ => \/-(root))
  }

  /**
   * @param reference reference with a pointer
   * @return
   */
  def resolveReference(reference: URI)(implicit rootURI: URI, loader: URI => String \/ Json): String \/ Json = {
    if (inprogress.contains(reference))
      -\/(s"found cyclic reference: $reference")
    else {
      // the refered Json document must be resolved as well
      loader(reference).flatMap {
        root =>
          JsonPointerDecodeJson(reference).flatMap(d => d(root.hcursor).toDisjunction.leftMap(_.toString())) flatMap {
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
      JsonSource.uri.json(uri)
  })

  def apply[T](addr: T)(implicit src:JsonSource[T]): String \/ Json =
    src.json(addr).flatMap(root => {
      val uri = src.uri(addr)
      local.resolvePointer(uri)(root, uri)
    })

}