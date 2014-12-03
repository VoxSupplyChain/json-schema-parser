package json.reference

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

  private def resolve(json: Json)(implicit root: Json): String \/ Json = {

    reference(json) map {
      _ flatMap {
        uri =>
          if (uri.toString.startsWith("#"))
            resolvePointer(uri)(root)
          else
            resolveReference(uri, fromURI(uri))
      }
    } getOrElse {
      \/-(
        json.withObject {
          (obj: JsonObject) =>
            obj.withJsons(i => resolve(i).valueOr(_ => i))
        }.withArray { array =>
          array.map(i => resolve(i).valueOr(_ => i))
        }
      )
    }

  }

  private def parseUri(s: String): String \/ URI = \/.fromEither(Exception.catching(classOf[URISyntaxException]).either(new URI(s))).leftMap(_.getMessage)

  private def reference(json: Json): Option[String \/ URI] =
    for {
      ref <- json.field("$ref")
      str <- ref.string
    } yield parseUri(str)


  def resolvePointer(reference: URI)(implicit root: Json): String \/ Json = {
    resolveReference(reference, \/-(root))
  }

  /**
   * @param reference reference with a pointer
   * @return
   */
  def resolveReference(reference: URI, loader: => String \/ Json): String \/ Json = {
    if (inprogress.contains(reference))
      -\/(s"cyclic dependency found: $reference")
    else {
      // the refered Json document must be resolved as well
      loader.flatMap {
        root =>
          JsonPointerDecodeJson(reference).flatMap(d => d(root.hcursor).toDisjunction.leftMap(_.toString())) flatMap {
            pointedNode =>
              val nestedResolver=new ReferenceResolver(inprogress.push(reference))
              nestedResolver(pointedNode)
          }

      }
    }
  }

  def fromURI(reference: URI): String \/ Json = {
    import scala.io.Source
    val html = if (reference.isAbsolute) Source.fromURL(reference.toURL) else Source.fromURI(reference)
    val s = html.mkString
    s.parse
  }

}


object ReferenceResolver {

  import Argonaut._

  val local: ReferenceResolver = new ReferenceResolver()

  def apply(root: Json): String \/ Json = local.resolvePointer(new URI("#"))(root)

  def apply(json: String): String \/ Json = json.parse.flatMap(apply)

}