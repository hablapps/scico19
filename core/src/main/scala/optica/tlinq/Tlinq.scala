package optica
package tlinq

trait Tlinq[Repr[_]] {

  def int(i: Int): Repr[Int]

  def bool(b: Boolean): Repr[Boolean]

  def string(s: String): Repr[String]

  def foreach[A, B](
    as: Repr[List[A]])(
    f: Repr[A] => Repr[List[B]]): Repr[List[B]]

  def where[A](c: Repr[Boolean])(as: Repr[List[A]]): Repr[List[A]]

  def yields[A](a: Repr[A]): Repr[List[A]]

  def product[A, B](a: Repr[A], b: Repr[B]): Repr[(A, B)]

  def nil[A]: Repr[List[A]]

  def subtract(x: Repr[Int], y: Repr[Int]): Repr[Int]

  def greaterThan(x: Repr[Int], y: Repr[Int]): Repr[Boolean]

  def equal[A](a1: Repr[A], a2: Repr[A]): Repr[Boolean]

  def and(p: Repr[Boolean], q: Repr[Boolean]): Repr[Boolean]

  def not(p: Repr[Boolean]): Repr[Boolean]

  def exists[A](f: Repr[List[A]]): Repr[Boolean]

  def ifs[A](b: Repr[Boolean], t: Repr[A], e: Repr[A]): Repr[A]

  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]

  def app[A, B](f: Repr[A => B])(a: Repr[A]): Repr[B]

  def some[A](a: Repr[A]): Repr[Option[A]]

  def none[A]: Repr[Option[A]]

  def ofold[A, B](oa: Repr[Option[A]])(z: Repr[B], f: Repr[A => B]): Repr[B]
}

object Tlinq {

  implicit object RTlinq extends interpreter.R

  implicit object ToStrinTlinq extends interpreter.ToString

  trait Syntax {

    implicit class TlinqOps[Repr[_], A](a: Repr[A])(implicit Q: Tlinq[Repr]) {
      
      def ===(a2: Repr[A]): Repr[Boolean] = Q.equal(a, a2)

      def ***[B](b: Repr[B]): Repr[(A, B)] = Q.product(a, b)
    }

    implicit class TlinqIntOps[Repr[_]](i: Repr[Int])(implicit Q: Tlinq[Repr]) {
      
      def -(j: Repr[Int]): Repr[Int] = Q.subtract(i, j)

      def >(j: Repr[Int]): Repr[Boolean] = Q.greaterThan(i, j)
    }

    implicit class TlinqBooleanOps[Repr[_]](
        b: Repr[Boolean])(implicit
        Q: Tlinq[Repr]) {
      
      def &&(b2: Repr[Boolean]): Repr[Boolean] = Q.and(b, b2)
    }
  }

  object syntax extends Syntax
}

