package optica
package sql

case class SSelect(select: SqlSelect, from: SqlFrom, where: Option[SqlExp]) {
  override def toString: String =
    (new interpreter.ToString).sqlToString(this)
}

sealed abstract class SqlFrom
case class SFrom(ts: List[SqlTable]) extends SqlFrom

sealed abstract class SqlTable
case class STable(t: Table, v: Var, js: List[SqlJoin]) extends SqlTable

sealed abstract class SqlJoin
case class SJoin(t: Table, v: Var) extends SqlJoin
case class SEqJoin(t: Table, v: Var, cond: SqlEqJoinCond) extends SqlJoin

sealed abstract class SqlEqJoinCond
case class SOn(l: SProj, r: SProj) extends SqlEqJoinCond
case class SUsing(fn: FieldNme) extends SqlEqJoinCond

sealed abstract class SqlSelect
case class SList(es: List[SField]) extends SqlSelect

sealed abstract class SqlExp
case class SAll(e: String) extends SqlExp
case class SField(e: SqlExp, fn: FieldNme) extends SqlExp
case class SProj(v: Var, fn: FieldNme) extends SqlExp
case class SBinOp(op: String, l: SqlExp, r: SqlExp) extends SqlExp
case class SUnOp(op: String, e: SqlExp) extends SqlExp
case class SCons(v: String) extends SqlExp
case class SExists(sel: SSelect) extends SqlExp

