package optica
package symantics

import concrete._

trait FoldAct[Repr[_], Obs[_]] {

  def getAll[S, A](fl: Repr[Fold[S, A]]): Obs[S => List[A]]
}

object FoldAct {

  trait Syntax {

    implicit class FoldOps[Repr[_], Obs[_], S, A](
        fl: Repr[Fold[S, A]])(implicit 
        ev: FoldAct[Repr, Obs]) {

      def getAll: Obs[S => List[A]] = ev.getAll(fl)
    }
  }

  object syntax extends Syntax
}

