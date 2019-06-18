package optica
package symantics

import concrete._

trait AffineFoldSym[Repr[_]] {

  def id_af[S]: Repr[AffineFold[S, S]]

  def andThen_af[S, A, B](
    u: Repr[AffineFold[S, A]],
    d: Repr[AffineFold[A, B]]): Repr[AffineFold[S, B]]

  def filtered[S](p: Repr[Getter[S, Boolean]]): Repr[AffineFold[S, S]]

  def as_afl[S, A](gt: Repr[Getter[S, A]]): Repr[AffineFold[S, A]]
}

object AffineFoldSym {

  trait Syntax {

    implicit class AffineFoldOps[Repr[_], S, A](
        af: Repr[AffineFold[S, A]])(implicit 
        ev: AffineFoldSym[Repr]) {
          
      def >>>[B](other: Repr[AffineFold[A, B]]): Repr[AffineFold[S, B]] =
        ev.andThen_af(af, other)
    }

    implicit def gt_as_afl[Repr[_], S, A](
        af: Repr[Getter[S, A]])(implicit
        ev: AffineFoldSym[Repr]): Repr[AffineFold[S, A]] =
      ev.as_afl(af)
  }

  object syntax extends Syntax
}

