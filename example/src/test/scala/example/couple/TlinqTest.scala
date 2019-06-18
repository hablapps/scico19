package example
package couple

import optica._
import symantics.interpreter.Wrap
import symantics.Optica.tlinqOptica

import _root_.org.scalatest._
import couple.interpreter.{Nested, Schema}
import Logic._

class TlinqTest extends FlatSpec with Matchers {

  implicit val _1 = symantics.Optica.tlinqOptica[λ[x => x]]
  implicit val _2 = Model.tlinqModel[λ[x => x]]

  val differencesTlinq: Couples => List[(String, Int)] =
    differences[Wrap[λ[x => x], ?], λ[x => x]]

  "Optica" should "translate differences into a function" in {
    differencesTlinq(Nested[λ[x => x]]) shouldBe List("Alex" -> 5, "Cora" -> 2) 
  }

  it should "work exactly the same as the raw T-Linq differences query" in {
    differencesTlinq(Nested[λ[x => x]]) shouldBe Schema.differences[λ[x => x]]
  }

  implicit val _3 = symantics.Optica.tlinqOptica[λ[x => Int => String]]
  implicit val _4 = Model.tlinqModel[λ[x => Int => String]]
  val _5 = implicitly[tlinq.Tlinq[λ[x => Int => String]]]

  val differencesTlinqShow: Int => String =
    differences[Wrap[λ[x => Int => String], ?], λ[x => Int => String]]

  _5.app(differencesTlinqShow)(Nested[λ[x => Int => String]])(0)

  // generates...
  //
  // (λx0 -> for (x1 <- (λx1 -> for (x2 <- (λx2 -> x2)(x1)) for (x3 <- (λx3 ->
  // (λx4 -> if ((λx5 -> (λx6 -> (λx7 -> x7.age)((λx7 -> x7.her)(x6)))(x5) >
  // (λx6 -> (λx7 -> x7.age)((λx7 -> x7.him)(x6)))(x5))(x4)) Some(x4) else
  // None)(x3).fold(Nil)((λx4 -> yield x4)))(x2)) yield x3)(x0)) for (x2 <-
  // (λx2 -> (λx3 -> Some((λx4 -> ((λx5 -> (λx6 -> x6.name)((λx6 ->
  // x6.her)(x5)))(x4), (λx5 -> (λx6 -> (λx7 -> x7.age)((λx7 ->
  // x7.her)(x6)))(x5) - (λx6 -> (λx7 -> x7.age)((λx7 ->
  // x7.him)(x6)))(x5))(x4)))(x3)))(x2).fold(Nil)((λx3 -> yield x3)))(x1))
  // yield x2)(for (x0 <- table_couple) for (x1 <- table_person) for (x2 <-
  // table_person) where x0.her == x2.name && x0.him == x1.name yield
  // Couple(Person(x2.name, x2.age), Person(x1.name, x1.age)))
}

