package json.reference

import java.io.IOException
import java.net.{URISyntaxException, URI}

import argonaut.{Argonaut, JsonObject, Json}
import argonaut.Argonaut._
import json.pointer.{JsonPointerDecodeJson, JsonPointer}

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.util.Try
import scala.util.control.Exception
import scalaz._


/**
 * Implementation JSON-Reference resolver as described in http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03
 * @param inprogress current resolution stack, to help tracking cyclic dependencies
 */
class ReferenceResolver(inprogress: Stack[URI] = Stack(new URI(""))) {

  def apply(json: Json): String \/ Json = {
    resolve(json)(json)
  }

  private def resolve(json: Json)(implicit root: Json): String \/ Json = ReferenceTraverser(json.hcursor) {
    uri =>
      val resolved = if (uri.toString.startsWith("#"))
        resolvePointer(uri)(root)
      else
        resolveReference(uri, fromURI)

      resolved leftMap (cause => s"reference $uri not found: $cause")
  }

  def resolvePointer(reference: URI)(implicit root: Json): String \/ Json = {
    resolveReference(reference, _ => \/-(root))
  }

  /**
   * @param reference reference with a pointer
   * @return
   */
  def resolveReference(reference: URI, loader: URI => String \/ Json): String \/ Json = {
    if (inprogress.contains(reference))
      -\/(s"found cyclic reference: $reference")
    else {
      // the refered Json document must be resolved as well
      loader(reference).flatMap {
        root =>
          println(s"$reference in $root")
          JsonPointerDecodeJson(reference).flatMap(d => d(root.hcursor).toDisjunction.leftMap(_.toString())) flatMap {
            pointedNode =>
              val nestedResolver = new ReferenceResolver(inprogress.push(reference))
              nestedResolver.resolve(pointedNode)(root)
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
      case e: IOException => -\/(e.getMessage)
    }
  }

}


object ReferenceResolver {

  import Argonaut._

  val local: ReferenceResolver = new ReferenceResolver()

  def apply(root: Json): String \/ Json = local.resolvePointer(new URI("#"))(root)

  def apply(json: String): String \/ Json = json.parse.flatMap(apply)

}