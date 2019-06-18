package example.couple

import optica.concrete._

case class Couple(her: Person, him: Person)
case class Person(name: String, age: Int)

trait Model[Repr[_]] {
  def couples: Repr[Fold[Couples, Couple]]
  def her: Repr[Getter[Couple, Person]]
  def him: Repr[Getter[Couple, Person]]
  def name: Repr[Getter[Person, String]]
  def age: Repr[Getter[Person, Int]]
}

object Model {

  implicit object R extends interpreter.RModel

  implicit object XQueryModel extends interpreter.XQueryModel

  implicit object TripletFunModel extends interpreter.TripletFunModel

  import interpreter.{TlinqModel, Nested}
  import optica.tlinq._

  implicit def tlinqModel[Repr[_] : Tlinq : Nested] = new TlinqModel[Repr]
}

