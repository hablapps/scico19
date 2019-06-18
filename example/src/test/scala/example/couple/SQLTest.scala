package example
package couple

import scalaz._, scalaz.Scalaz._
import cats.effect.IO
import _root_.org.scalatest._
import optica._
import sql._
import triplet._
import doobie._
import doobie.implicits._
import Logic._

class SQLTest extends FlatSpec with Matchers {

  type Obs[_] = TypeNme ==>> FieldNme => Error \/ SSelect

  val differencesSQL: TypeNme ==>> FieldNme => Error \/ SSelect =
    differences[λ[x => TripletFun], Obs]

  "Optica" should "translate difference into a SELECT statement" in {
    differencesSQL(==>>("Person" -> "name")).map(_.toString) shouldBe \/-(
      "SELECT w.name, (w.age - m.age) FROM Couple AS c INNER JOIN Person AS w ON c.her = w.name INNER JOIN Person AS m ON c.him = m.name WHERE (w.age > m.age)")
  }

  it should "be a correct query for a db" in {

    val people = List(
      Couple(Person("Alex", 60), Person("Bert", 55)),
      Couple(Person("Cora", 33), Person("Drew", 31)),
      Couple(Person("Edna", 21), Person("Fred", 60)))

    Utils.transactor.use(transIO =>
      for {
        select <- differencesSQL(==>>("Person" -> "name")).fold(IO.raiseError, IO.pure)
        _ <- Utils.prepareCoupleEnviroment(people).transact(Utils.xa)
        result <- Query0[(String, Int)](select.toString).to[List].transact(transIO)
      } yield result).unsafeRunSync shouldBe List("Alex" -> 5, "Cora" -> 2)
  }
}
