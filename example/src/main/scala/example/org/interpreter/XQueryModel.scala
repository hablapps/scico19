package example.org
package interpreter

import optica.xquery._

class XQueryModel extends Model[λ[x => XQuery]] {
  
  val departments = Name("department")

  val dpt = Name("dpt")

  val employees = Name("employee")

  val tasks = Name("task")

  val tsk = Name("tsk")
}

