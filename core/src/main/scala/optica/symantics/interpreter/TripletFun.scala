package optica
package symantics
package interpreter

import Function.const
import monocle.function.all._
import scalaz._, Scalaz._

import triplet._, triplet.interpreter.ToSQL
import sql._

trait TripletFunGetterSym extends GetterSym[λ[x => TripletFun]] {

  def id_gt[S] = identity

  def andThen_gt[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def fork_gt[S, A, B](l: TripletFun, r: TripletFun) = 
    merge3With(l, r)(_ ++ _, _ lmerge _, _ ++ _)

  def like[S, A: Base](a: A) = first[Triplet, List[TExpr]].set(List(Like(a)))

  def not[S](b: TripletFun) = b andThen first[Triplet, List[TExpr]].modify {
    case List(e) => List(Not(e))  
    case _ => throw new Error("should never happen")
  }

  private def binary(
      x: TripletFun, y: TripletFun)(
      op: (TExpr, TExpr) => TExpr): TripletFun = 
    merge3With(x, y)(
      { case (List(e1), List(e2)) => List(op(e1, e2))
        case _ => throw new Error("should never happen") },
      _ lmerge _, _ ++ _) 

  def equal[S, A: Base](x: TripletFun, y: TripletFun) = binary(x, y)(Eq)
    
  def greaterThan[S](x: TripletFun, y: TripletFun) = binary(x, y)(GreaterThan)

  def subtract[S](x: TripletFun, y: TripletFun) = binary(x, y)(Sub)
}

trait TripletFunGetterAct 
    extends GetterAct[λ[x => TripletFun],
                      λ[x => TypeNme ==>> FieldNme => Error \/ SSelect]] {

  def get[S, A](gt: TripletFun) = 
    const(new Error("unsupported action: `get`").left)
}

trait TripletFunAffineFoldSym extends AffineFoldSym[λ[x => TripletFun]] {

  def id_af[S] = identity

  def andThen_af[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def filtered[S](p: TripletFun) = {
    case (s, f, w) => p((s, f, Set.empty)) match {
      case (e, f2, _) => (s, f2, w ++ e)
    }
  }

  def as_afl[S, A](gt: TripletFun) = gt
}

trait TripletFunAffineFoldAct 
    extends AffineFoldAct[λ[x => TripletFun],
                          λ[x => TypeNme ==>> FieldNme => Error \/ SSelect]] {

  def preview[S, A](af: TripletFun) = 
    const(new Error("unsupported action: `preview`").left)
}

trait TripletFunFoldSym extends FoldSym[λ[x => TripletFun]] {

  def id_fl[S] = identity

  def andThen_fl[S, A, B](u: TripletFun, d: TripletFun) = u andThen d

  def nonEmpty[S, A](fl: TripletFun) = {
    case (s, f, w) => 
      (List(NonEmpty(fl((s, f.map(x => (x._1, false)), Set.empty)))), f, w)
  }

  def as_fl[S, A](afl: TripletFun) = afl
}

trait TripletFunFoldAct 
    extends FoldAct[λ[x => TripletFun], 
                    λ[x => TypeNme ==>> FieldNme => Error \/ SSelect]] {

  def getAll[S, A](fl: TripletFun) = ToSQL.toSql(fl, _)
}

class TripletFunSym 
    extends Optica[λ[x => TripletFun], 
                   λ[x => TypeNme ==>> FieldNme => Error \/ SSelect]]
    with TripletFunGetterSym with TripletFunGetterAct
    with TripletFunAffineFoldSym with TripletFunAffineFoldAct
    with TripletFunFoldSym with TripletFunFoldAct {

  def entity(ot: OpticType, vn: String): TripletFun = {
    case (List(Path(xs)), f, w) => {
      val f2 = index[VarTree, List[OpticType], VarTree](xs).modify { it =>
        it.copy(forest = it.forest.insertWith((_, e) => e, ot, ITree((vn, true))))
      }(f)
      (List(Path(ot :: xs)), f2, w)
    }
    case _ => throw new Error("should never happen")
  }

  def base(ot: OpticType): TripletFun = first[Triplet, List[TExpr]].modify { 
    case List(e: Path) => List(Proj(e, ot))
    case _ => throw new Error("should never happen")
  }
}

