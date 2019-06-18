package optica
package xquery

sealed abstract class XQuery {
  override def toString = (new interpreter.ToString).xqueryToString(this)
}
case object Document extends XQuery
case object Self extends XQuery
case class Seq(p: XQuery, q: XQuery) extends XQuery
case class Name(s: String) extends XQuery
case class Filter(p: XQuery) extends XQuery
case class Func(op: String, p: XQuery) extends XQuery
case class Oper(op: String, p: XQuery, q: XQuery) extends XQuery
case class PInt(i: Int) extends XQuery
case class PBool(b: Boolean) extends XQuery
case class PString(s: String) extends XQuery
case class Tuple(fst: XQuery, snd: XQuery) extends XQuery

