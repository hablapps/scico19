package example.org

import optica.concrete._

case class Department(dpt: String, employees: List[Employee])
case class Employee(emp: String, tasks: List[Task])
case class Task(tsk: String)

trait Model[Repr[_]] {

  def departments: Repr[Fold[Org, Department]]

  def dpt: Repr[Getter[Department, String]]

  def employees: Repr[Fold[Department, Employee]]

  def tasks: Repr[Fold[Employee, Task]]

  def tsk: Repr[Getter[Task, String]]
}

object Model {

  implicit object R extends interpreter.RModel

  implicit object xQueryModel extends interpreter.XQueryModel

  implicit object TripletFunModel extends interpreter.TripletFunModel

  import interpreter.{TlinqModel, Nested}
  import optica.tlinq._

  implicit def tlinqModel[Repr[_] : Tlinq : Nested] = new TlinqModel[Repr]
}

