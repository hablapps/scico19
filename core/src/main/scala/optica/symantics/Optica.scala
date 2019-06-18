package optica
package symantics

import concrete._

trait Optica_[Repr[_]] extends GetterSym[Repr]
    with AffineFoldSym[Repr]
    with FoldSym[Repr] {

  def empty[S, A](fl: Repr[Fold[S, A]]): Repr[Getter[S, Boolean]] =
    not(nonEmpty(fl))

  def any[S, A](
      fl: Repr[Fold[S, A]])(
      p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
    nonEmpty(andThen_fl(fl, as_fl(filtered(p))))

  def all[S, A](
      fl: Repr[Fold[S, A]])(
      p: Repr[Getter[A, Boolean]]): Repr[Getter[S, Boolean]] =
    empty(andThen_fl(fl, as_fl(filtered(not(p)))))

  def elem[S, A: Base](fl: Repr[Fold[S, A]])(a: A): Repr[Getter[S, Boolean]] =
    any(fl)(equal(id_gt, like(a)))
}

trait Optica[Repr[_], Obs[_]] extends Optica_[Repr]
    with GetterAct[Repr, Obs] 
    with AffineFoldAct[Repr, Obs]
    with FoldAct[Repr, Obs]

object Optica {

  implicit object ROptica extends interpreter.R

  implicit object XQueryOptica extends interpreter.XQuerySym

  implicit object TripletFunOptica extends interpreter.TripletFunSym

  implicit def tlinqOptica[Repr[_]: tlinq.Tlinq]: interpreter.TlinqSym[Repr] =
    new interpreter.TlinqSym[Repr]

  trait Syntax extends GetterSym.Syntax with GetterAct.Syntax
    with AffineFoldSym.Syntax with AffineFoldAct.Syntax
    with FoldSym.Syntax with FoldAct.Syntax

  object syntax extends Syntax
}

