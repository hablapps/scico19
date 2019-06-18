package example.couple
package interpreter

import optica.triplet._
import optica.symantics.Optica.TripletFunOptica.{base, entity}

class TripletFunModel extends Model[λ[x => TripletFun]] {

  val couples = entity(FoldType("couples", "Couples", "Couple"), "c")

  val her = entity(GetterType("her", "Couple", "Person"), "w")

  val him = entity(GetterType("him", "Couple", "Person"), "m")

  val name = base(GetterType("name", "Person", "String"))

  val age = base(GetterType("age", "Person", "Int"))
}

