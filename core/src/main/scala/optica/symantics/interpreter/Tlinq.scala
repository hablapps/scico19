package optica
package symantics
package interpreter

import tlinq._
import concrete._
import Base._

import Tlinq.syntax._

sealed abstract class Wrap[Repr[_], A]
case class WrapGetter[Repr[_], S, A](f: Repr[S => A]) 
  extends Wrap[Repr, Getter[S, A]]
case class WrapAffine[Repr[_], S, A](f: Repr[S => Option[A]]) 
  extends Wrap[Repr, AffineFold[S, A]]
case class WrapFold[Repr[_], S, A](f: Repr[S => List[A]]) 
  extends Wrap[Repr, Fold[S, A]]

trait TlinqGetterSym[Repr[_]] extends GetterSym[Wrap[Repr, ?]] {

  implicit val Q: Tlinq[Repr]
  import Q._
  
  def andThen_gt[S, A, B](
      u: Wrap[Repr, Getter[S, A]],
      d: Wrap[Repr, Getter[A, B]]) = (u, d) match {
    case (WrapGetter(f), WrapGetter(g)) => 
      WrapGetter(lam(s => app(g)(app(f)(s))))
  }

  def fork_gt[S, A, B](
      l: Wrap[Repr, Getter[S, A]],
      r: Wrap[Repr, Getter[S, B]]) = (l, r) match {
    case (WrapGetter(f), WrapGetter(g)) =>
      WrapGetter(lam(s => app(f)(s) *** app(g)(s)))
  }

  def id_gt[S] = WrapGetter(lam(identity))

  def like[S, A](a: A)(implicit B: Base[A]) = WrapGetter(lam[S, A](_ =>
    B match {
      case IntWitness => int(a)
      case BooleanWitness => bool(a)
      case StringWitness => string(a)
    }))

  def not[S](b: Wrap[Repr, Getter[S, Boolean]]) = b match {
    case WrapGetter(f) => WrapGetter(lam(s => Q.not(app(f)(s))))
  }

  def equal[S, A: Base](
      x: Wrap[Repr, Getter[S, A]],
      y: Wrap[Repr, Getter[S, A]]) = (x, y) match {
    case (WrapGetter(f), WrapGetter(g)) =>
      WrapGetter(lam(s => app(f)(s) === app(g)(s)))
  }

  def greaterThan[S](
      x: Wrap[Repr, Getter[S, Int]], 
      y: Wrap[Repr, Getter[S, Int]]) = (x, y) match {
    case (WrapGetter(f), WrapGetter(g)) =>
      WrapGetter(lam(s => app(f)(s) > app(g)(s)))
  }

  def subtract[S](
      x: Wrap[Repr, Getter[S, Int]], 
      y: Wrap[Repr, Getter[S, Int]]) = (x, y) match {
    case (WrapGetter(f), WrapGetter(g)) =>
      WrapGetter(lam(s => app(f)(s) - app(g)(s)))
  }
}

trait TlinqGetterAct[Repr[_]] extends GetterAct[Wrap[Repr, ?], Repr] {

  def get[S, A](gt: Wrap[Repr, Getter[S, A]]) = gt match {
    case WrapGetter(f) => f
  }
}

trait TlinqAffineFoldSym[Repr[_]] extends AffineFoldSym[Wrap[Repr, ?]] {

  implicit val Q: Tlinq[Repr]
  import Q._
  
  def id_af[S] = WrapAffine(lam(some))

  def andThen_af[S, A, B](
      u: Wrap[Repr, AffineFold[S, A]], 
      d: Wrap[Repr, AffineFold[A, B]]) = (u, d) match {
    case (WrapAffine(f), WrapAffine(g)) =>
      WrapAffine(lam(s => ofold(app(f)(s))(none, lam(app(g)))))
  }

  def filtered[S](p: Wrap[Repr, Getter[S, Boolean]]) = p match {
    case WrapGetter(f) => WrapAffine(lam(s => ifs(app(f)(s), some(s), none)))
  }

  def as_afl[S, A](gt: Wrap[Repr, Getter[S, A]]) = gt match {
    case WrapGetter(f) => WrapAffine(lam(s => some(app(f)(s))))
  }
}

trait TlinqAffineFoldAct[Repr[_]] extends AffineFoldAct[Wrap[Repr, ?], Repr] {

  def preview[S, A](af: Wrap[Repr, AffineFold[S, A]]) = af match {
    case WrapAffine(f) => f
  }
}

trait TlinqFoldSym[Repr[_]] extends FoldSym[Wrap[Repr, ?]] {

  implicit val Q: Tlinq[Repr]
  import Q._
  
  def id_fl[S] = WrapFold(lam(yields))

  def andThen_fl[S, A, B](
      u: Wrap[Repr, Fold[S, A]], 
      d: Wrap[Repr, Fold[A, B]]) = (u, d) match {
    case (WrapFold(f), WrapFold(g)) =>
      WrapFold(lam(s => 
        foreach(app(f)(s))(a =>
          foreach(app(g)(a))(yields))))
  }

  def nonEmpty[S, A](fl: Wrap[Repr, Fold[S, A]]) = fl match {
    case WrapFold(f) => WrapGetter(lam(s => exists(app(f)(s))))
  }

  def as_fl[S, A](afl: Wrap[Repr, AffineFold[S, A]]) = afl match {
    case WrapAffine(f) => 
      WrapFold(lam(s => ofold(app(f)(s))(nil, lam(yields))))
  }
}

trait TlinqFoldAct[Repr[_]] extends FoldAct[Wrap[Repr, ?], Repr] {

  def getAll[S, A](fl: Wrap[Repr, Fold[S, A]]) = fl match {
    case WrapFold(f) => f
  }
}

class TlinqSym[Repr[_]](implicit val Q: Tlinq[Repr]) 
  extends Optica[Wrap[Repr, ?], Repr]
  with TlinqGetterSym[Repr] with TlinqGetterAct[Repr]
  with TlinqAffineFoldSym[Repr] with TlinqAffineFoldAct[Repr]
  with TlinqFoldSym[Repr] with TlinqFoldAct[Repr]

