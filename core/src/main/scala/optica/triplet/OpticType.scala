package optica
package triplet

import scalaz._, Scalaz._

sealed abstract class OpticType {
  val name: String 
  val from: String 
  val to: String
}

object OpticType {
  implicit val opticTypeOrder: Order[OpticType] = 
    Order.orderBy(ot => s"${ot.name}${ot.from}${ot.to}")
}

case class GetterType(name: String, from: String, to: String) extends OpticType

case class AffineFoldType(name: String, from: String, to: String) extends OpticType

case class FoldType(name: String, from: String, to: String) extends OpticType

