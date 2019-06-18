package optica
package tlinq
package interpreter

import io.getquill._

object ctx extends SqlMirrorContext(MirrorSqlDialect, Literal)
import ctx._

sealed abstract class QuillRepr[A]
case class QuillT[A](a: Quoted[A]) extends QuillRepr[A]
case class QuillL[A](a: Quoted[Query[A]]) extends QuillRepr[List[A]]

trait QuillTlinq extends Tlinq[QuillRepr] {

  def int(i: Int) = QuillT(quote { i })

  def bool(b: Boolean) = QuillT(quote { b })

  def string(s: String) = QuillT(quote { s })

  def foreach[A, B](
      as: QuillRepr[List[A]])(
      f: QuillRepr[A] => QuillRepr[List[B]]) = as match {
    case QuillL(qa) => QuillL(quote {
      for {
        a <- qa
        b <- f(QuillT(a)) match { 
          case QuillL(qb) => qb 
          case _ => throw new Error("should never happen")
        }
      } yield b
    })
    case _ => throw new Error("should never happen")
  }

  def where[A](c: QuillRepr[Boolean])(as: QuillRepr[List[A]]) = (c, as) match {
    case (QuillT(c), QuillL(as)) => QuillL(quote {
      for {
        a <- as
        if c
      } yield a
    })
    case _ => throw new Error("should never happen")
  }

  def yields[A](a: QuillRepr[A]) = a match {
    case QuillT(a) => ???
    case _ => throw new Error("should never happen")
  }

  def product[A, B](a: QuillRepr[A], b: QuillRepr[B]) = (a, b) match {
    case (QuillT(x), QuillT(y)) => QuillT(quote { 
      val a: A = x
      val b: B = y
      (a, b)
    })
    case _ => throw new Error("should never happen")
  }

  def nil[A] = ???

  def subtract(x: QuillRepr[Int], y: QuillRepr[Int]) = (x, y) match {
    case (QuillT(a), QuillT(b)) => QuillT(quote { a - b })
  }

  def greaterThan(x: QuillRepr[Int], y: QuillRepr[Int]) = (x, y) match {
    case (QuillT(a), QuillT(b)) => QuillT(quote { a > b })
  }

  def equal[A](a1: QuillRepr[A], a2: QuillRepr[A]) = (a1, a2) match {
    case (QuillT(x), QuillT(y)) => QuillT(quote { x == y })
    case _ => throw new Error("should never happen")
  }

  def and(p: QuillRepr[Boolean], q: QuillRepr[Boolean]) = (p, q) match {
    case (QuillT(x), QuillT(y)) => QuillT(quote { x && y })
  }

  def not(p: QuillRepr[Boolean]) = p match {
    case QuillT(b) => QuillT(quote { !b })
  }

  def exists[A](f: QuillRepr[List[A]]) = f match {
    case QuillL(g) => QuillT(quote { g.nonEmpty })
    case _ => throw new Error("should never happen")
  }

  def ifs[A](b: QuillRepr[Boolean], t: QuillRepr[A], e: QuillRepr[A]) =
    (b, t, e) match {
      case (QuillT(c), QuillT(th), QuillT(el)) => QuillT(quote {
        // XXX: if (c) th else el
        if (true) th else el
      })
      case _ => throw new Error("should never happen")
    }

  def lam[A, B](f: QuillRepr[A] => QuillRepr[B]) = 
    QuillT(quote { a: A =>
      f(QuillT(a)) match {
        case QuillT(b) => b
        case _ => throw new Error("should never happen")
      }
    })

  def app[A, B](f: QuillRepr[A => B])(a: QuillRepr[A]) = (f, a) match {
    case (QuillT(g), QuillT(x)) => QuillT(quote { g(x) })
    case _ => throw new Error("should never happen")
  }

  def some[A](a: QuillRepr[A]) = ???

  def none[A] = ???

  def ofold[A, B](
      oa: QuillRepr[Option[A]])(
      z: QuillRepr[B], 
      f: QuillRepr[A => B]) =
    ???
}

class Quill extends QuillTlinq

