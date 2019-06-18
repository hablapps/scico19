package example
package org

import scalaz._, Scalaz._
import cats.effect.IO
import _root_.org.scalatest._
import doobie._
import doobie.implicits._
import optica._
import sql._
import triplet._
import Logic._

class SQLTest extends FlatSpec with Matchers {

  type Obs[_] = TypeNme ==>> FieldNme => Error \/ SSelect

  def expertiseSQL(u: String): TypeNme ==>> FieldNme => Error \/ SSelect =
    expertise[λ[x => TripletFun], Obs](u)

  val keys = ==>>("Department" -> "dpt", "Employee" -> "emp")

  "Optica" should "translate expertise into a SELECT statement" in {
    expertiseSQL("abstract")(keys).map(_.toString) shouldBe 
      \/-("""SELECT d.dpt FROM Department AS d WHERE NOT(EXISTS(SELECT e.* FROM Employee AS e WHERE (NOT(EXISTS(SELECT t.tsk FROM Task AS t WHERE ((t.tsk = "abstract") AND (e.emp = t.emp)))) AND (d.dpt = e.dpt))))""")
  }

  it should "work with a database" in {

    val people = List(
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

    Utils.transactor.use(transIO =>
      for {
        select <- expertiseSQL("abstract")(keys).fold(IO.raiseError, IO.pure)
        _ <- Utils.prepareOrgEnviroment(people).transact(Utils.xa)
        result <-  Query0[String](select.toString).to[List].transact(transIO)
      } yield result).unsafeRunSync shouldBe List("Quality", "Research")
  }
}

