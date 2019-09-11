package optica
package xquery
package interpreter

class ToString {

  def xqueryToString(xq: XQuery): String = xq match {
    case Document => "/"
    case Seq(Document, p) => s"/$p"
    case Seq(p, q@(Filter(_) | Seq(Filter(_), _))) => s"$p$q"
    case Seq(p, q) => s"$p/$q"
    case Name(s) => s
    case Filter(p) => s"[$p]"
    case Self => "."
    case Element(tag, contents @ _*) => 
      s"""<$tag>${contents.map {
        case e: Element => xqueryToString(e)
        case x => s"{${xqueryToString(x)}}"
      }.mkString}</$tag>"""
    case Func(op, p) => s"$op($p)"
    case Oper(op, p, q) => s"$p $op $q"
    case PInt(i) => s"$i"
    case PBool(b) => s"$b()"
    case PString(s) => s""""$s""""
  }
}

