package optica

import scalaz._
import sql._

package object `triplet` {

  type VarTree = ITree[OpticType, (String, Boolean)]

  type Triplet = (List[TExpr], VarTree, Set[TExpr])

  type Endo[A] = A => A

  type Choose[A] = (A, A) => A

  type TripletFun = Endo[Triplet]

  def mergeWith[A](e1: Endo[A], e2: Endo[A])(f: Choose[A]): Endo[A] = { a => 
    f(e1(a), e2(a))
  }

  def merge3With[A, B, C](
      e1: Endo[(A, B, C)], e2: Endo[(A, B, C)])(
      f: Choose[A], g: Choose[B], h: Choose[C]): Endo[(A, B, C)] = 
    mergeWith(e1, e2) {
      case ((a1, b1, c1), (a2, b2, c2)) => (f(a1, a2), g(b1, b2), h(c1, c2))
    }

  implicit class TripletFunOps(tf: TripletFun) {
    def toSQL(keys: TypeNme ==>> FieldNme): Error \/ SSelect = 
      interpreter.ToSQL.toSql(tf, keys)
  }
}

