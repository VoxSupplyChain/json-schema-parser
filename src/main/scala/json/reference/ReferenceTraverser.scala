package json.reference

import java.net.{URISyntaxException, URI}

import argonaut.Argonaut._
import argonaut.{HCursor, ACursor, Json}

import scala.util.control.Exception
import scalaz._
import scala.collection.immutable.Stack

trait ReferenceTraverser {

  sealed trait TraverseOp

  object TCheck extends TraverseOp

  object TUp extends TraverseOp

  sealed case class TObject(fields: Set[JsonField]) extends TraverseOp

  sealed case class TArray(lastIndex: Int) extends TraverseOp

  type TraverseResult = Validation[String, Json]
  type TraverseStack = Stack[TraverseOp]
  type TraverseState = TraverseResult \/ TraverseStack

  def initialState: TraverseState = \/-(Stack(TCheck))

  private def withStack(tuple: (TraverseStack, ACursor)): (\/-[TraverseStack], ACursor) = (\/-(tuple._1), tuple._2)

  private def parseUri(s: String): String \/ URI = \/.fromEither(Exception.catching(classOf[URISyntaxException]).either(new URI(s))).leftMap(_.getMessage)

  private def jsonReference(json: Json): Option[String \/ URI] =
    for {
      ref <- json.field("$ref")
      str <- ref.string
    } yield parseUri(str)

  def treeTraverser(resolve: URI => Json)(state: TraverseState, hc: HCursor): (TraverseState, ACursor) = {
    state.fold(
      s => (state, hc.failedACursor),
      fs => {
        fs.headOption map {
          case TCheck =>
            jsonReference(hc.focus) match {
              case Some(\/-(ref)) =>
                val replaced: HCursor = hc.set(resolve(ref))
                withStack {
                  (fs.tail, replaced.acursor)
                }
              case Some(-\/(err)) =>
                (-\/(Failure(err)), hc.failedACursor)
              case None =>
                withStack {
                  hc.focus.arrayOrObject(
                    (fs.tail, hc.acursor),
                    array =>
                      (fs.tail.push(TArray(array.length - 1)), hc.acursor),
                    obj =>
                      (fs.tail.push(TObject(obj.fieldSet)), hc.acursor)
                  )
                }
            }
          case TArray(index) =>
            withStack {
              if (index >= 0) {
                (fs.tail.push(TArray(index - 1), TUp, TCheck), hc.downN(index))
              }
              else
                (fs.tail, hc.acursor)
            }
          case TObject(fields) =>
            withStack {
              if (fields.isEmpty)
                (fs.tail, hc.acursor)
              else {
                (fs.tail.push(TObject(fields.tail), TUp, TCheck), hc.downField(fields.head))
              }
            }
          case TUp =>
            withStack {
              (fs.tail, hc.up)
            }
        } getOrElse {
          (-\/(Success(hc.focus)), hc.failedACursor)
        }
      }
    )
  }

}

object ReferenceTraverser extends ReferenceTraverser {
  def apply(hcursor: HCursor)(resolve: URI => Json): TraverseResult = hcursor.traverseUntilDone(initialState)(treeTraverser(resolve)) match {
    case -\/(v) => v
    case \/-(s) => Failure("json traversal is incomplete")
  }
}
