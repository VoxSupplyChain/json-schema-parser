package json.reference

import java.net.{URI, URISyntaxException}

import argonaut.Argonaut._
import argonaut.{ACursor, HCursor, Json}

import scala.util.control.Exception
import scalaz._

/**
 * Traverser that replaces all $ref with a resolved Json value.
 */
trait ReferenceTraverser {

  private type TraverseState = TraverseOp

  def resolve(ref: URI): String \/ Json

  private sealed trait TraverseOp {
    def next(hc: HCursor): (TraverseState, ACursor)
  }

  private sealed case class TCheck(tail: TraverseOp) extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) = jsonReference(hc.focus) match {
      case Some(\/-(ref)) =>
        resolve(ref).fold(
          f => (TResult(-\/(f)), hc.acursor),
          resolvedNode =>
            (tail, hc.set(resolvedNode).acursor)
        )
      case Some(-\/(err)) =>
        (TResult(-\/(err)), hc.acursor)
      case None =>
        hc.focus.arrayOrObject(
          (tail, hc.acursor),
          array =>
            (TArray(array.length - 1, tail), hc.acursor),
          obj =>
            (TObject(obj.fieldSet, tail), hc.acursor)
        )
    }
  }

  private sealed case class TUp(tail: TraverseOp) extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) = (tail, hc.up)
  }

  private sealed case class TObject(fields: Set[JsonField], tail: TraverseOp) extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) = if (fields.isEmpty)
      (tail, hc.acursor)
    else
      (TCheck(
        TUp(
          this.copy(fields.tail)
        )
      ), hc.downField(fields.head))
  }

  private sealed case class TArray(index: Int, tail: TraverseOp) extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) =
      if (index >= 0)
        (TCheck(
          TUp(
            this.copy(index - 1)
          )
        ), hc.downN(index))
      else
        (tail, hc.acursor)
  }

  private sealed case class TResult(result: String \/ Json) extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) = (this, hc.failedACursor)
  }

  private object TReturn extends TraverseOp {
    override def next(hc: HCursor): (TraverseState, ACursor) = (TResult(\/-(hc.focus)), hc.acursor)
  }

  private def parseUri(s: String): String \/ URI =
    \/.fromEither(Exception.catching(classOf[URISyntaxException]).either(new URI(s)))
      .leftMap(_.getMessage)

  private def jsonReference(json: Json): Option[String \/ URI] =
    for {
      ref <- json.field("$ref")
      str <- ref.string
    } yield parseUri(str)

  private def treeTraverser(state: TraverseState, hc: HCursor): (TraverseState, ACursor) = state.next(hc)

  def traverse(hcursor: HCursor): String \/ Json = {
    val init: TraverseOp = TCheck(TReturn)
    hcursor.traverseUntilDone(init)(treeTraverser) match {
      case TResult(result) => result
      case _ => -\/("json traversal is incomplete")
    }
  }

}

object ReferenceTraverser {

  /**
   * Removes all $ref references.
   */
  object NullTraverser extends ReferenceTraverser {
    override def resolve(uri: URI): String \/ Json = \/-(jNull)
  }

}