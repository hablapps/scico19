package optica
package concrete

import scalaz._, Scalaz._, Kleisli._
import Getter.syntax._

case class Fold[S, A](getAll: S => List[A])

object Fold {

  implicit object FoldCat extends Category[Fold] {

    def id[A]: Fold[A, A] = Fold(ask[List, A])

    def compose[A, B, C](f: Fold[B, C], g: Fold[A, B]): Fold[A, C] =
      Fold(kleisli(g.getAll) >>> kleisli(f.getAll))
  }

  def nonEmpty[S, A](fl: Fold[S, A]): Getter[S, Boolean] =
    Getter(fl.getAll(_).nonEmpty)

  def empty[S, A](fl: Fold[S, A]): Getter[S, Boolean] =
    Getter.not(nonEmpty(fl))

  def all[S, A](fl: Fold[S, A])(p: Getter[A, Boolean]): Getter[S, Boolean] =
    empty(fl >>> AffineFold.filtered[A](Getter.not(p)))

  def any[S, A](fl: Fold[S, A])(p: Getter[A, Boolean]): Getter[S, Boolean] =
    Getter.not(all(fl)(Getter.not(p)))

  def elem[S, A: Equal](fl: Fold[S, A])(a: A): Getter[S, Boolean] =
    any(fl)(Getter.GetterCat.id[A] === a)

  implicit def fromAffineFold[S, A](afl: AffineFold[S, A]): Fold[S, A] =
    Fold(afl.preview(_).toList)

  implicit def fromGetter[S, A](getter: Getter[S, A]): Fold[S, A] =
    Fold.fromAffineFold(AffineFold.fromGetter(getter))

  trait Syntax {

    implicit class FoldOps[S, A](fl: Fold[S, A]) {

      def nonEmpty: Getter[S, Boolean] =
        Fold.nonEmpty(fl)

      def empty: Getter[S, Boolean] =
        Fold.empty(fl)

      def all(p: Getter[A, Boolean]): Getter[S, Boolean] =
        Fold.all(fl)(p)

      def any(p: Getter[A, Boolean]): Getter[S, Boolean] =
        Fold.any(fl)(p)

      def elem(a: A)(implicit Eq: Equal[A]): Getter[S, Boolean] =
        Fold.elem(fl)(a)
    }
  }

  object syntax extends Syntax
}

