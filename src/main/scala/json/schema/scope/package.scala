package json.schema

import java.net.{URI, URISyntaxException}

import argonaut.Argonaut._
import argonaut.{ACursor, HCursor, Json}
import json.pointer.JsonPointer
import json.reference.ReferenceResolver

import scala.util.control.Exception
import scalaz.\/

package object scope {

  private[scope] trait JsonTraverser {

    // current scope uri
    type State
    type TraverseState = (State, TraverseOp)

    trait TraverseOp {
      def next(state: State, hc: HCursor): (TraverseState, ACursor)
    }

    private[scope] def TCheck(tail: TraverseOp): TraverseOp

    private[scope] def TReturn: TraverseOp

    private[scope] case class TUp(tail: TraverseOp) extends TraverseOp {
      override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, tail), hc.up)
    }

    private[scope] case class TArray(index: Int, tail: TraverseOp) extends TraverseOp {
      override def next(state: State, hc: HCursor): (TraverseState, ACursor) =
        if (index >= 0)
          ((state, TCheck(
            TUp(
              this.copy(index - 1)
            )
          )), hc.downN(index))
        else
          ((state, tail), hc.acursor)
    }

    private[scope] case class TObject(fields: Set[JsonField], tail: TraverseOp) extends TraverseOp {
      override def next(state: State, hc: HCursor): (TraverseState, ACursor) = if (fields.isEmpty)
        ((state, tail), hc.acursor)
      else
        ((state,
          TCheck(
            TUp(
              this.copy(fields.tail)
            )
          )), hc.downField(fields.head))
    }


    private[scope] def treeTraverser(state: TraverseState, hc: HCursor): (TraverseState, ACursor) =
      state._2.next(state._1, hc)

    private[scope] def childScope(parent: URI, sub: URI) = JsonPointer.resolveAsPointer(parent, sub)

    private[scope] def parseUri(s: String): String \/ URI =
      \/
        .fromEither(Exception.catching(classOf[URISyntaxException]).either(new URI(s)))
        .leftMap(_.getMessage)

    private[scope] def getId(json: Json): Option[String \/ URI] =
      for {
        ref <- json.field("id")
        str <- ref.string
      } yield parseUri(str)

    private[scope] def getRef(json: Json): Option[String \/ URI] =
      for {
        ref <- json.field("$ref")
        str <- ref.string
      } yield parseUri(str)


  }

}
