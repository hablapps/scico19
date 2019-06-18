package optica
package concrete

import Function.const
import scalaz._, Scalaz._

case class Getter[S, A](get: S => A)

object Getter {

  implicit object GetterCat extends CategoryWithProduct[Getter, Tuple2] {

    def id[A]: Getter[A, A] = Getter(identity)

    def compose[A, B, C](f: Getter[B, C], g: Getter[A, B]): Getter[A, C] =
      Getter(g.get >>> f.get)

    def pi1[A, B]: Getter[(A, B), A] = Getter(_._1)

    def pi2[A, B]: Getter[(A, B), B] = Getter(_._2)

    def fork[S, A, B](f: Getter[S, A], g: Getter[S, B]): Getter[S, (A, B)] =
      Getter(s => (f.get(s), g.get(s)))
  }

  def like[S, A](a: A): Getter[S, A] = Getter(const(a))

  def not[S](b: Getter[S, Boolean]): Getter[S, Boolean] = b >>> Getter(!_)

  def equal[S, A: Equal](x: Getter[S, A], y: Getter[S, A]): Getter[S, Boolean] =
    Getter(s => x.get(s) === y.get(s))

  def greaterThan[S](x: Getter[S, Int], y: Getter[S, Int]): Getter[S, Boolean] =
    Getter(s => x.get(s) > y.get(s))

  def subtract[S](x: Getter[S, Int], y: Getter[S, Int]): Getter[S, Int] =
    Getter(s => x.get(s) - y.get(s))

  trait GetterSyntax {

    implicit def liftLike[S, A](a: A): Getter[S, A] = like(a)

    implicit class GetterSEqOps[S, A: Equal](gt: Getter[S, A]) {
      def ===(other: Getter[S, A]): Getter[S, Boolean] = equal(gt, other)
    }

    implicit class GetterSIntOps[S](gt: Getter[S, Int]) {
      def >(other: Getter[S, Int]): Getter[S, Boolean] = greaterThan(gt, other)
      def -(other: Getter[S, Int]): Getter[S, Int] = subtract(gt, other)
    }
  }

  object syntax extends GetterSyntax
}

