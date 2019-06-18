package optica
package triplet
package interpreter

import scalaz._, Scalaz._
import monocle.function.all._
import sql._

object ToSQL {

  def zero: Triplet = (List(Path()), ITree(("/", true)), Set.empty)

  type Stack[A] = ReaderT[Error \/ ?, TypeNme ==>> FieldNme, A]

  def toSql(
      sem: TripletFun,
      keys: TypeNme ==>> FieldNme): Error \/ SSelect =
    toSql[Stack](sem(zero))(implicitly, implicitly).run(keys)

  def toSql[M[_]: Monad](
      tri: Triplet)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[SSelect] =
    for {
      s <- selToSql(tri._1, tri._2)(MR, ME)
      f <- tabToSql(tri._2)(MR, ME)
      w <- whrToSql(tri._3, tri._2)(MR, ME)
    } yield SSelect(s, f, w)

  private def selToSql[M[_]](
      sel: List[TExpr],
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[SqlSelect] = {
    implicit val M: Monad[M] = MR
    sel
      .traverse(treeToExpr(_, vars)(MR, ME))
      .map(_.map(e => SField(e, "")))
      .map(SList)
  }

  private def tabToSql[M[_]: Monad](
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[SqlFrom] =
    // FIXME: this will fail when no inner table is created (get)
    // => tabToSql should return an optional table
    vars.subtree(! _.forest.filter(_.tag._2).isEmpty).get.forest.toList match {
      case (ot, it@ITree((v, l), _)) :: xs =>
        xs.foldLeft(seqJoinToSql(v, it)(MR)) {
          case (acc, (ot2, it2@ITree((v2, l2), _))) =>
            for {
              x <- acc
              y <- seqJoinToSql(v2, it2)(MR)
            } yield x ::: (SJoin(ot2.to, v2) :: y)
        }.map(xs => SFrom(List(STable(ot.to, v, xs))))
      case _ => ME.raiseError(
        new Error(s"Can't translate semantic with multiple roots: $vars"))
    }

  private def condToSql(
      v1: String, n1: FieldNme,
      v2: String, n2: FieldNme): SqlEqJoinCond =
    if (n1 == n2) SUsing(n1) else SOn(SProj(v1, n1), SProj(v2, n2))

  private def seqJoinToSql[M[_]](
      topv: Symbol,
      vars: VarTree)(implicit
      MR: MonadReader[M, TypeNme ==>> FieldNme]): M[List[SqlJoin]] =
    vars.forest.toList.foldLeft(MR.point(List.empty[SqlJoin])) {
      case (acc, (ot, child@ITree((v, l), _))) =>
        for {
          xs1 <- acc
          x   <- joinToSql(topv, ot.name, v, ot)
          xs2 <- seqJoinToSql(ot.name, child)
        } yield (xs1 :+ x) ++ xs2
    }

  private def joinToSql[M[_]](
      topv: Symbol,
      nme: String,
      v: Symbol,
      ot: OpticType)(implicit
      MR: MonadReader[M, TypeNme ==>> FieldNme]): M[SqlJoin] =
    MR.ask >>= (keys => MR.point(SEqJoin(ot.to, v, ot match {
      // TODO: unsafe get, move to MonadError
      case FoldType(_, from, _) => SUsing(keys.lookup(from).get)
      case other => condToSql(topv, nme, v, keys.lookup(other.to).get)
    })))

  private def whrToSql[M[_]: Monad](
      whr: Set[TExpr],
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[Option[SqlExp]] =
    for {
      oe1 <- whrColToSql(whr, vars)(MR, ME)
      oe2 <- whrExtToSql(vars)(MR, ME)
    } yield (oe1, oe2) match {
      case (None, other) => other
      case (other, None) => other
      case (Some(e1), Some(e2)) => Some(SBinOp("AND", e1, e2))
    }

  private def whrColToSql[M[_]: Monad](
      whr: Set[TExpr],
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[Option[SqlExp]] =
    whr.foldLeft(MR.point(Option.empty[SqlExp])) {
      case (acc, t) => acc >>= {
        case None => treeToExpr(t, vars)(MR, ME).map(Some(_))
        case Some(e) =>
          treeToExpr(t, vars)(MR, ME).map(x => Some(SBinOp("AND", e, x)))
      }
    }

  private def whrExtToSql[M[_]: Monad](
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[Option[SqlExp]] =
    if (! vars.tag._2)
      for {
        keys <- MR.ask
        vars2 = vars.subtree(! _.forest.filter(_.tag._2).isEmpty).get
        oe <- vars2.forest.fold(MR.point(Option.empty[SqlExp])) {
          case (FoldType(name, from, to), ITree((v, _), _), acc) =>
            keys.lookup(from) match {
              case Some(pk) => {
                val e = SBinOp("=",
                  SProj(vars2.tag._1, pk),
                  SProj(v, pk))
                acc >>= (x => MR.point(x.fold(Option(e))(l => Option(SBinOp("AND", l, e)))))
              }
              case None =>
                ME.raiseError[Option[SqlExp]](new Error(s"Can't find '$from' in keys '$keys'"))
            }
          // TODO: we're just covering Folds
          case _ => ME.raiseError(new Error(s"TODO: support for other optics"))
        }
      } yield oe
    else MR.point(Option.empty)

  private def binOpToExpr[M[_]: Monad](
      op: String,
      l: TExpr,
      r: TExpr,
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[SqlExp] =
    (treeToExpr(l, vars)(MR, ME) |@| treeToExpr(r, vars)(MR, ME)) {
      case (el, er) => SBinOp(op, el, er)
    }

  private def treeToExpr[M[_]: Monad](
      t: TExpr,
      vars: VarTree)(
      MR: MonadReader[M, TypeNme ==>> FieldNme],
      ME: MonadError[M, Error]): M[SqlExp] = t match {
    case Path(xs) =>
      index[VarTree, List[OpticType], VarTree](xs)
        .getOption(vars)
        .fold[M[SqlExp]](
          ME.raiseError(new Error(s"Path '$xs' doesn't exist in '$vars'")))(
          it => MR.point[SqlExp](SAll(it.tag._1)))
    case Proj(Path(xs), ot) =>
      index[VarTree, List[OpticType], VarTree](xs)
        .getOption(vars)
        .fold[M[SqlExp]](
          ME.raiseError(new Error(s"Path '$xs' doesn't exist in '$vars'")))(
          it => MR.point[SqlExp](SProj(it.tag._1, ot.name)))
    case Sub(l, r) => binOpToExpr("-", l, r, vars)(MR, ME)
    case GreaterThan(l, r) => binOpToExpr(">", l, r, vars)(MR, ME)
    case Not(b) => treeToExpr(b, vars)(MR, ME).map(SUnOp("NOT", _))
    case Eq(l, r) => binOpToExpr("=", l, r, vars)(MR, ME)
    case Like(s: String) => MR.point(SCons(s""""$s""""))
    case Like(x) => MR.point(SCons(x.toString))
    case NonEmpty(tri) => toSql(tri)(MR, ME).map(SExists(_))
  }
}

