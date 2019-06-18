package optica
package sql
package interpreter

class ToString {

  def sqlToString(sql: SSelect): String = {
    val sel = selToString(sql.select)
    val frm = frmToString(sql.from)
    val whr = sql.where.fold("")(e => s" WHERE ${expToString(e)}")
    s"SELECT $sel FROM $frm$whr"
  }

  private def selToString(sel: SqlSelect): String = sel match {
    case SList(es) => es.map(expToString).mkString(", ")
  }

  private def frmToString(frm: SqlFrom): String = frm match {
    case SFrom(ts) => ts.map(tabToString).mkString(", ")
  }

  private def tabToString(tab: SqlTable): String = tab match {
    case STable(t, v, js) => {
      val s = (if (js.nonEmpty) " " else "") ++
        js.map(joinToString).mkString(" ")
      s"$t AS $v$s"
    }
  }

  private def joinToString(join: SqlJoin): String = join match {
    case SJoin(t, v) => s"JOIN $t AS $v"
    case SEqJoin(t, v, c) => s"INNER JOIN $t AS $v ${joinCondToString(c)}"
  }

  private def joinCondToString(cond: SqlEqJoinCond): String = cond match {
    case SOn(l, r) => s"ON ${expToString(l)} = ${expToString(r)}"
    case SUsing(fn) => s"USING ($fn)"
  }

  private def expToString(exp: SqlExp): String = exp match {
    case SAll(e) => s"$e.*"
    case SField(e, "") => s"${expToString(e)}"
    case SField(e, fn) => s"${expToString(e)} AS $fn"
    case SProj(v, fn) => s"$v.$fn"
    case SBinOp(op, l, r) => s"(${expToString(l)} $op ${expToString(r)})"
    case SUnOp(op, e) => s"$op(${expToString(e)})"
    case SCons(v) => v
    case SExists(ssel) => s"EXISTS(${sqlToString(ssel)})"
  }
}

