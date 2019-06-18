package example
package org

import _root_.org.scalatest._
import Logic._

class RTest extends FlatSpec with Matchers {

  val data: Org = List(
    Department("Product", List(
      Employee("Alex", List(Task("build"))),
      Employee("Bert", List(Task("build"))))),
    Department("Quality", List.empty),
    Department("Research", List(
      Employee("Cora", List(Task("abstract"), Task("build"), Task("design"))),
      Employee("Drew", List(Task("abstract"), Task("design"))),
      Employee("Edna", List(Task("abstract"), Task("call"), Task("design"))))),
    Department("Sales", List(
      Employee("Fred", List(Task("call"))))))

  def expertiseR(u: String): Org => List[String] =
    expertise[λ[x => x], λ[x => x]](u)

  "Optica" should "translate expertise into a fold" in {
    expertiseR("abstract")(data) shouldBe List("Quality", "Research")
  }
}

