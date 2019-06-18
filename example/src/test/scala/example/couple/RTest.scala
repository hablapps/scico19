package example
package couple

import _root_.org.scalatest._
import Logic._

class RTest extends FlatSpec with Matchers {

  val data: Couples = List(
    Couple(Person("Alex", 60), Person("Bert", 55)),
    Couple(Person("Cora", 33), Person("Drew", 31)),
    Couple(Person("Edna", 21), Person("Fred", 60)))

  val differencesR: Couples => List[(String, Int)] = 
    differences[λ[x => x], λ[x => x]]

  "Optica" should "translate differences into a fold" in {
    differencesR(data) shouldBe List("Alex" -> 5, "Cora" -> 2) 
  }
}

