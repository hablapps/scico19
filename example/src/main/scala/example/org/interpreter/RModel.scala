package example.org
package interpreter

import optica.concrete._

class RModel extends Model[λ[x => x]] {
  val departments = Fold(identity)
  val dpt = Getter(_.dpt)
  val employees = Fold(_.employees)
  val tasks = Fold(_.tasks)
  val tsk = Getter(_.tsk)
}

