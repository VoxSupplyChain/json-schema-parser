package json.schema.scope

import java.net.{URI, URISyntaxException}

import argonaut.Argonaut._
import argonaut.{ACursor, HCursor, Json}
import json.reference.ReferenceResolver

import scala.util.control.Exception
import scalaz.{-\/, \/, \/-}

trait ExpandReferences {

  private def childScope(parent: URI, sub: URI) = ReferenceResolver.resolve(parent, sub)

  // current scope uri
  type State = URI

  private sealed trait TraverseOp {
    def next(state: State, hc: HCursor): (TraverseState, ACursor)
  }

  private sealed case class TCheck(tail: TraverseOp) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = getId(hc.focus) match {
      case Some(-\/(err)) =>
        ((state, TResult(-\/(err))), hc.acursor)
      case Some(\/-(id)) =>

        val newScope = childScope(state, id)

        // expand id element
        val ac: ACursor = hc.downField("id").withFocus(_ => jString(childScope(state, id).toString)).up

        hc.focus.arrayOrObject(
          ((state, tail), ac),
          array =>
            ((newScope, TArray(array.length - 1, tail)), ac),
          obj =>
            ((newScope, TObject(obj.fieldSet, tail)), ac)
        )

      case None =>

        // expand $ref element
        val ac: ACursor = getRef(hc.focus) match {
          case Some(\/-(id)) => hc.downField("$ref").withFocus(_ => jString(childScope(state, id).toString)).up
          case _ => hc.acursor
        }

        hc.focus.arrayOrObject(
          ((state, tail), ac),
          array =>
            ((state, TArray(array.length - 1, tail)), ac),
          obj =>
            ((state, TObject(obj.fieldSet, tail)), ac)
        )

    }
  }

  private sealed case class TUp(tail: TraverseOp) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, tail), hc.up)
  }

  private sealed case class TObject(fields: Set[JsonField], tail: TraverseOp) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = if (fields.isEmpty)
      ((state, tail), hc.acursor)
    else
      (
        (
          state,
          TCheck(
            TUp(
              this.copy(fields.tail)
            )
          ))
        , hc.downField(fields.head)
        )
  }

  private sealed case class TArray(index: Int, tail: TraverseOp) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) =
      if (index >= 0)
        (
          (state, TCheck(
            TUp(
              this.copy(index - 1)
            )
          )
            ), hc.downN(index)
          )
      else
        ((state, tail), hc.acursor)
  }

  private sealed case class TResult(result: String \/ Json) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, this), hc.failedACursor)
  }

  private object TReturn extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, TResult(\/-(hc.focus))), hc.acursor)
  }

  private type TraverseState = (State, TraverseOp)

  private def parseUri(s: String): String \/ URI = \/.fromEither(Exception.catching(classOf[URISyntaxException]).either(new URI(s))).leftMap(_.getMessage)

  private def getId(json: Json): Option[String \/ URI] =
    for {
      ref <- json.field("id")
      str <- ref.string
    } yield parseUri(str)

  private def getRef(json: Json): Option[String \/ URI] =
    for {
      ref <- json.field("$ref")
      str <- ref.string
    } yield parseUri(str)


  private def treeTraverser(state: TraverseState, hc: HCursor): (TraverseState, ACursor) = state._2.next(state._1, hc)

  def expand(rootScope: URI, hcursor: HCursor): String \/ Json = {
    val init: (State, TraverseOp) = (rootScope, TCheck(TReturn))
    hcursor.traverseUntilDone(init)(treeTraverser) match {
      case (state, TResult(result)) => result
      case _ => -\/("json traversal is incomplete")
    }
  }

}

object ExpandReferences extends ExpandReferences
