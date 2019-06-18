package example.org
package interpreter

import optica.triplet._
import optica.symantics.Optica.TripletFunOptica.{base, entity}

class TripletFunModel extends Model[λ[x => TripletFun]] {

  val departments = entity(FoldType("departments", "Org", "Department"), "d")

  val dpt = base(GetterType("dpt", "Department", "String"))

  val employees = entity(FoldType("employees", "Department", "Employee"), "e")

  val tasks = entity(FoldType("tasks", "Employee", "Task"), "t")

  val tsk = base(GetterType("tsk", "Task", "String"))
}

