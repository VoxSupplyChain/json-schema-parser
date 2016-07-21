package json.schema.scope

import java.net.{URI, URISyntaxException}

import argonaut.Argonaut._
import argonaut.{ACursor, HCursor, Json}
import json.reference.ReferenceResolver

import scala.util.control.Exception
import scalaz.{-\/, \/, \/-}

trait ScopeDiscovery extends JsonTraverser {

  private[scope] case class ScopeState(scope: URI, scopes: Map[URI, Json])

  type State = ScopeState

  private[scope] def TCheck(tail: TraverseOp): TraverseOp = new TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = getId(hc.focus) match {
      case Some(-\/(err)) =>
        ((state, TResult(-\/(err))), hc.acursor)
      case Some(\/-(id)) =>

        val newScope = childScope(state.scope, id)

        hc.focus.arrayOrObject(
          ((state, tail), hc.acursor),
          array =>
            ((ScopeState(newScope, state.scopes + (newScope -> hc.focus)), TArray(array.length - 1, tail)), hc.acursor),
          obj =>
            ((ScopeState(newScope, state.scopes + (newScope -> hc.focus)), TObject(obj.fieldSet, tail)), hc.acursor)
        )

      case None =>
        val ac = hc.acursor
        hc.focus.arrayOrObject(
          ((state, tail), ac),
          array =>
            ((state, TArray(array.length - 1, tail)), ac),
          obj =>
            ((state, TObject(obj.fieldSet, tail)), ac)
        )

    }
  }


  private[scope] case class TResult(result: String \/ Map[URI, Json]) extends TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, this), hc.failedACursor)
  }

  private[scope] def TReturn: TraverseOp = new TraverseOp {
    override def next(state: State, hc: HCursor): (TraverseState, ACursor) = ((state, TResult(\/-(state.scopes))), hc.acursor)
  }

  /**
   * Builds a map of identified json objects (json objects with ID).
   * @param rootScope
   * @param json
   * @return  error or a map of ids to json objects.
   */
  def scopes(rootScope: URI, json: Json): String \/ Map[URI, Json] = {
    val init: (State, TraverseOp) = (ScopeState(rootScope, Map(rootScope -> json)), TCheck(TReturn))
    json.hcursor.traverseUntilDone(init)(treeTraverser) match {
      case (state, TResult(result)) => result
      case _ => -\/("json traversal is incomplete")
    }
  }

}

object ScopeDiscovery extends ScopeDiscovery
