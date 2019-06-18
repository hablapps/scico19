package optica
package symantics

import concrete._

trait FoldSym[Repr[_]] {

  def id_fl[S]: Repr[Fold[S, S]]

  def andThen_fl[S, A, B](
    u: Repr[Fold[S, A]],
    d: Repr[Fold[A, B]]): Repr[Fold[S, B]]

  def nonEmpty[S, A](fl: Repr[Fold[S, A]]): Repr[Getter[S, Boolean]]

  def empty[S, A](fl: Repr[Fold[S, A]]): Repr[Getter[S, Boolean]]

  def any[S, A](
    fl: Repr[Fold[S, A]])(
    p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]]

  def all[S, A](
    fl: Repr[Fold[S, A]])(
    p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]]

  def elem[S, A: Base](fl: Repr[Fold[S, A]])(a: A): Repr[Getter[S, Boolean]]

  def as_fl[S, A](afl: Repr[AffineFold[S, A]]): Repr[Fold[S, A]]
}

object FoldSym {

  trait Syntax {

    implicit class FoldOps[Repr[_], S, A](
        fl: Repr[Fold[S, A]])(implicit 
        ev: FoldSym[Repr]) {

      def >>>[B](other: Repr[Fold[A, B]]): Repr[Fold[S, B]] = 
        ev.andThen_fl(fl, other)

      def all(p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
        ev.all(fl)(p)

      def elem(a: A)(implicit B: Base[A]): Repr[Getter[S, Boolean]] =
        ev.elem(fl)(a)
    }

    implicit def afl_as_fl[Repr[_], S, A](
        af: Repr[AffineFold[S, A]])(implicit
        ev: FoldSym[Repr]): Repr[Fold[S, A]] =
      ev.as_fl(af)

    implicit def gt_as_fl[Repr[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev1: FoldSym[Repr], 
        ev2: AffineFoldSym[Repr]): Repr[Fold[S, A]] =
      ev1.as_fl(ev2.as_afl(gt))
  }

  object syntax extends Syntax
}

