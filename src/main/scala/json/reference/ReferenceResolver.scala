package json.reference

import java.io.File
import java.net.URI

import argonaut.Argonaut._
import argonaut.{Argonaut, Json}
import json.pointer.JsonPointerDecodeJson

import scala.collection.immutable.Stack
import scala.io.Source
import scala.util.control.NonFatal
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 * @param inprogress current resolution stack, to help tracking cyclic dependencies
 */
class ReferenceResolver(inprogress: Stack[URI] = Stack(new URI(""))) {

  def relative(parent: URI, sub: URI) = {
    val resolved = parent.resolve(sub)
    if (resolved.getFragment == null || resolved.getFragment.isEmpty) resolved.resolve("#") else resolved
  }


  private def resolve(json: Json)(implicit root: Json, rootURI: URI): String \/ Json = ReferenceTraverser(json.hcursor) {
    uri =>
      val resolved = if (uri.toString.startsWith("#"))
        resolvePointer(uri)(root, rootURI)
      else
        resolveReference(relative(rootURI, uri))(rootURI, fromURI)

      resolved leftMap (cause => s"reference $uri not found: $cause")
  }

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
              val nestedResolver = new ReferenceResolver(inprogress.push(reference))
              nestedResolver.resolve(pointedNode)(root, rootURI)
          }

      }
    }
  }

  def fromURI(reference: URI): String \/ Json = {
    try {
      import scala.io.Source
      val html = if (reference.isAbsolute) Source.fromURL(reference.toURL) else Source.fromURI(reference)
      val s = html.mkString
      s.parse
    } catch {
      case NonFatal(e) => -\/(e.getMessage)
    }
  }

}


object ReferenceResolver {

  import argonaut.Argonaut._

  val local: ReferenceResolver = new ReferenceResolver()

  def apply(root: Json): String \/ Json = local.resolvePointer(new URI("#"))(root, new URI(""))

  def apply(file: File): String \/ Json =
    scala.io.Source.fromFile(file).mkString.parse.flatMap(root => local.resolvePointer(file.toURI)(root, file.toURI))

  def apply(uri: URI): String \/ Json =
    toSource(uri).mkString.parse.flatMap(root => local.resolvePointer(uri)(root, uri))

  def apply(json: String): String \/ Json = json.parse.flatMap(apply)

  private def toSource(uri: URI): Source = try {
    scala.io.Source.fromURI(uri)
  } catch {
    case NonFatal(e) => scala.io.Source.fromURL(uri.toURL)
  }
}