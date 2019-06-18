package optica
package triplet

sealed abstract class TExpr
case class Path(xs: List[OpticType] = List.empty[OpticType]) extends TExpr
case class Proj(path: Path, ot: OpticType) extends TExpr
case class Like[A](a: A) extends TExpr
case class Not(b: TExpr) extends TExpr
case class Eq(a: TExpr, b: TExpr) extends TExpr
case class GreaterThan(x: TExpr, y: TExpr) extends TExpr
case class Sub(x: TExpr, y: TExpr) extends TExpr
case class NonEmpty(tri: Triplet) extends TExpr

