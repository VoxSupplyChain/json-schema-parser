package json.util

import argonaut.{ACursor, HCursor}

import scalaz.{Kleisli, State}

trait ArgonautTraverse {

  implicit class HCursorTraverse(hc: HCursor) {

    def traverseABreak[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, Option[ACursor]]): State[X, Boolean] =
      State(x => {
        @annotation.tailrec
        def spin(z: X, d: HCursor): (X, Boolean) = {
          val (q, k) = r run d run z
          k match {
            case None => (q, true)
            case Some(a) =>
              a.hcursor match {
                case None => (q, false)
                case Some(hcursor) => spin(q, hcursor)
              }
          }
        }

        spin(x, hc)
      })

    def traverseA[X](r: Kleisli[({type λ[α] = State[X, α]})#λ, HCursor, ACursor]): State[X, Boolean] =
      traverseABreak(r map (Some(_)))

    /**
      * Traverse until either `f` does not return a cursor or the cursor did not succeed,
      * accumulating X at each sterp
      */
    def traverseUntil[X](init: X)(f: (X, HCursor) => (X, Option[ACursor])): X =
      traverseABreak[X](Kleisli[({type λ[+α] = State[X, α]})#λ, HCursor, Option[ACursor]](c => State((x: X) => f(x, c)))) exec init

    /**
      * Traverse until `f` returns a cursor that did not succeed,
      * accumulating X at each step
      */
    def traverseUntilDone[X](init: X)(f: (X, HCursor) => (X, ACursor)): X =
      traverseUntil(init)((x, c) => {
        val (xx, cc) = f(x, c);
        (xx, Some(cc))
      })
  }

}