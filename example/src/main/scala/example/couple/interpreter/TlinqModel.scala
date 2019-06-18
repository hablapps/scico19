package example.couple
package interpreter

import optica.tlinq._
import optica.symantics.interpreter._

import Tlinq.syntax._
import Schema.syntax._

case class CoupleRel(her: String, him: String)
case class PersonRel(name: String, age: Int)

class TlinqModel[Repr[_]](implicit Q: Tlinq[Repr], N: Nested[Repr]) 
    extends Model[Wrap[Repr, ?]] {
  import Q._, Nested.syntax._

  val couples = WrapFold(lam(identity))

  val her = WrapGetter(lam(c => c.her))

  val him = WrapGetter(lam(c => c.him))

  val name = WrapGetter(lam(p => p.name))

  val age = WrapGetter(lam(p => p.age))
}
      
trait Schema[Repr[_]] {

  def table_couple: Repr[List[CoupleRel]]

  def her(c: Repr[CoupleRel]): Repr[String]
  
  def him(c: Repr[CoupleRel]): Repr[String]

  def table_person: Repr[List[PersonRel]]

  def name(p: Repr[PersonRel]): Repr[String]
  
  def age(p: Repr[PersonRel]): Repr[Int]
}

object Schema {
  
  def differences[Repr[_]](implicit
      Q: Tlinq[Repr],
      S: Schema[Repr]): Repr[List[(String, Int)]] = {
    import Q._, S._
    foreach(table_couple)(c =>
    foreach(table_person)(w =>
    foreach(table_person)(m =>
    where((c.her === w.name) && (c.him === m.name) && (w.age > m.age))(
    yields(w.name *** (w.age - m.age))))))
  }

  implicit object RSchema extends Schema[λ[x => x]] {

    def table_person = List(
      PersonRel("Alex", 60), 
      PersonRel("Bert", 55), 
      PersonRel("Cora", 33),
      PersonRel("Drew", 31),
      PersonRel("Edna", 21),
      PersonRel("Fred", 60))

    def her(c: CoupleRel) = c.her

    def him(c: CoupleRel) = c.him

    def table_couple = List(
      CoupleRel("Alex", "Bert"),
      CoupleRel("Cora", "Drew"),
      CoupleRel("Edna", "Fred"))

    def name(p: PersonRel) = p.name

    def age(p: PersonRel) = p.age
  }

  implicit object ToStringSchema extends Schema[λ[x => Int => String]] {

    def table_couple = _ => "table_couple"
    
    def her(c: Int => String) = i => s"${c(i)}.her"

    def him(c: Int => String) = i => s"${c(i)}.him"

    def table_person = _ => "table_person"
    
    def name(p: Int => String) = i => s"${p(i)}.name"

    def age(p: Int => String) = i => s"${p(i)}.age"
  }

  trait Syntax {

    implicit class CoupleRelOps[Repr[_]](
        c: Repr[CoupleRel])(implicit
        S: Schema[Repr]) {
      def her = S.her(c)
      def him = S.him(c)
    }

    implicit class PersonOps[Repr[_]](
        p: Repr[PersonRel])(implicit
        S: Schema[Repr]) {
      def name = S.name(p)
      def age  = S.age(p)
    }
  }

  object syntax extends Syntax
}

trait Nested[Repr[_]] {

  def Couple(her: Repr[Person], him: Repr[Person]): Repr[Couple]

  def her(c: Repr[Couple]): Repr[Person]

  def him(c: Repr[Couple]): Repr[Person]

  def Person(name: Repr[String], age: Repr[Int]): Repr[Person]

  def name(p: Repr[Person]): Repr[String]

  def age(p: Repr[Person]): Repr[Int]
}

object Nested {

  def apply[Repr[_]](implicit 
      Q: Tlinq[Repr],
      S: Schema[Repr],
      N: Nested[Repr]): Repr[Couples] = {
    import Q._, S._, N.{Couple, Person}
    foreach(table_couple)(c =>
    foreach(table_person)(m =>
    foreach(table_person)(w =>
    where(c.her === w.name && c.him === m.name)(
    yields(Couple(Person(w.name, w.age), Person(m.name, m.age)))))))
  }

  import syntax._

  def under50_d[Repr[_]](implicit
      Q: Tlinq[Repr],
      S: Schema[Repr],
      N: Nested[Repr]): Repr[List[String]] = {
    import Q._
    foreach(apply)(c =>
    where(c.her.age > int(50))(
    yields(c.her.name)))
  }

  implicit object RNested extends Nested[λ[x => x]] {

    def Couple(her: Person, him: Person) = example.couple.Couple(her, him)

    def her(c: Couple) = c.her

    def him(c: Couple) = c.him

    def Person(name: String, age: Int) = example.couple.Person(name, age)

    def name(p: Person) = p.name

    def age(p: Person) = p.age
  }

  implicit object ToStringNested extends Nested[λ[x => Int => String]] {

    def Couple(her: Int => String, him: Int => String) = { i =>
      s"Couple(${her(i)}, ${him(i)})"
    }

    def her(c: Int => String) = i => s"${c(i)}.her"

    def him(c: Int => String) = i => s"${c(i)}.him"

    def Person(name: Int => String, age: Int => String) = { i =>
      s"Person(${name(i)}, ${age(i)})"
    }

    def name(p: Int => String) = i => s"${p(i)}.name"
    
    def age(p: Int => String) = i => s"${p(i)}.age"
  }

  trait Syntax {

    implicit class CoupleOps[Repr[_]](
        c: Repr[Couple])(implicit
        N: Nested[Repr]) {
      def her = N.her(c)
      def him = N.him(c)
    }

    implicit class PersonOps[Repr[_]](
        p: Repr[Person])(implicit
        N: Nested[Repr]) {
      def name = N.name(p)
      def age = N.age(p)
    }
  }

  object syntax extends Syntax
}

