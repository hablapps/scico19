package example.couple
package interpreter

import optica.xquery._

class XQueryModel extends Model[λ[x => XQuery]] {

  val couples = Name("couple")

  val her = Name("her")
  
  val him = Name("him")
  
  val name = Name("name")

  val age = Name("age")
}

