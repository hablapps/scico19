package optica
package symantics

import concrete._

trait AffineFoldAct[Repr[_], Obs[_]] {

  def preview[S, A](af: Repr[AffineFold[S, A]]): Obs[S => Option[A]]
}

object AffineFoldAct {

  trait Syntax {

    implicit class AffineFoldOps[Repr[_], Obs[_], S, A](
        af: Repr[AffineFold[S, A]])(implicit 
        ev: AffineFoldAct[Repr, Obs]) {

      def preview: Obs[S => Option[A]] = ev.preview(af)
    }
  }

  object syntax extends Syntax
}

