package optica
package symantics

sealed abstract class Base[A]

object Base {
  implicit object IntWitness extends Base[Int]
  implicit object BooleanWitness extends Base[Boolean]
  implicit object StringWitness extends Base[String]
}

