package optica
package symantics

import concrete.Getter

trait GetterSym[Repr[_]] {

  def id_gt[S]: Repr[Getter[S, S]]

  def andThen_gt[S, A, B](
    u: Repr[Getter[S, A]],
    d: Repr[Getter[A, B]]): Repr[Getter[S, B]]

  def fork_gt[S, A, B](
    l: Repr[Getter[S, A]],
    r: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]]

  def like[S, A: Base](a: A): Repr[Getter[S, A]]

  def not[S](b: Repr[Getter[S, Boolean]]): Repr[Getter[S, Boolean]]

  def equal[S, A: Base](
    x: Repr[Getter[S, A]],
    y: Repr[Getter[S, A]]): Repr[Getter[S, Boolean]]

  def greaterThan[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]]

  def subtract[S](
    x: Repr[Getter[S, Int]],
    y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]]
}

object GetterSym {

  trait Syntax {

    implicit class GetterOps[Repr[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev: GetterSym[Repr]) {

      def >>>[B](other: Repr[Getter[A, B]]): Repr[Getter[S, B]] =
        ev.andThen_gt(gt, other)

      def ***[B](other: Repr[Getter[S, B]]): Repr[Getter[S, (A, B)]] =
        ev.fork_gt(gt, other)
    }

    implicit class GetterBaseOps[Repr[_], S, B: Base](
        gt: Repr[Getter[S, B]])(implicit
        ev: GetterSym[Repr]) {

      def ===(other: Repr[Getter[S, B]]): Repr[Getter[S, Boolean]] =
        ev.equal(gt, other)
    }

    implicit class GetterArithOps[Repr[_], S](
        gt: Repr[Getter[S, Int]])(implicit
        ev: GetterSym[Repr]) {

      def >(y: Repr[Getter[S, Int]]): Repr[Getter[S, Boolean]] =
        ev.greaterThan(gt, y)

      def -(y: Repr[Getter[S, Int]]): Repr[Getter[S, Int]] =
        ev.subtract(gt, y)
    }

    implicit def liftLike[Repr[_], S, B: Base](
        b: B)(implicit
        ev: GetterSym[Repr]): Repr[Getter[S, B]] =
      ev.like(b)
  }

  object syntax extends Syntax
} 

