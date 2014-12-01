package json.schema.parser

import argonaut._

import scala.annotation.tailrec
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader
import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}


case class JsonPointer(head: JsonPointerStep, tail: Option[JsonPointer]) {
  override def toString: String = {
    val t = tail.fold("")(_.toString)
    s"$head / $t"
  }
}

object JsonPointer extends Parsers {

  type Elem = Char

  val separator = elem('/')

  val digit = elem('0') |
    elem('1') |
    elem('2') |
    elem('3') |
    elem('4') |
    elem('5') |
    elem('6') |
    elem('7') |
    elem('8') |
    elem('9')

  val escapedSeparator = elem('~') ~ elem('1') ^^ {
    case _ => '/'
  }

  val escapedTilde = elem('~') ~ elem('0') ^^ {
    case _ => '~'
  }

  val numeric = rep1(digit) ^^ {
    case list => JsonPointerNumericStep(list.mkString.toInt)
  }

  val notSeparator = not(separator) ~ elem("notSeparator", _ => true) ^^ {
    case _ ~ c => c
  }

  val string = rep1(escapedSeparator | escapedTilde | notSeparator) ^^ {
    case list => JsonPointerStringStep(list.mkString)
  }

  val step = separator ~> (numeric | string)

  def parser: Parser[Try[JsonPointer]] = phrase(rep1(step)) ^^ {
    case x :: xs => TrySuccess(stepsToJsonPointer(x :: xs))
    case Nil => TryFailure(new IllegalArgumentException("Failed parsing json pointer"))
  }

  val root: JsonPointer = JsonPointer(JsonPointerRootStep, None)

  def apply(pointer: String): Try[JsonPointer] = {
    if (pointer.isEmpty)
      TrySuccess(root)
    else
      parser(new CharSequenceReader(pointer)) match {
        case Success(steps, _) => steps
        case NoSuccess(error, _) => TryFailure(new IllegalArgumentException(error))
      }
  }

  private def stepsToJsonPointer(steps: Seq[JsonPointerStep]): JsonPointer = {
    (steps.dropRight(1) :\ JsonPointer(steps.last, None)) { (m, n) =>
      JsonPointer(m, Some(n))
    }
  }

}

sealed trait JsonPointerStep

object JsonPointerRootStep extends JsonPointerStep {
  override def toString: String = "[root]"
}

final case class JsonPointerStringStep(key: String) extends JsonPointerStep {
  override def toString: String = "field " + key
}

final case class JsonPointerNumericStep(key: Int) extends JsonPointerStep {
  override def toString: String = "index " + key.toString
}

