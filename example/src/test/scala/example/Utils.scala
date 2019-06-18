package example

import cats.effect.{ContextShift, IO, Resource}
import doobie.util.update.Update0
import doobie._
import doobie.hikari.HikariTransactor
import doobie.implicits._
import example.`couple`.Couples
import example.org.{Department, Employee}
import cats.implicits._
import example.couple.Person

import scala.concurrent.ExecutionContext

object Utils {

  private def dropTable(table: String) =
    Update0(s"DROP TABLE IF EXISTS $table", None).run

  private val dropPersonTable = dropTable("Person")

  private val dropCoupleTable = dropTable("Couple")

  private val createPersonTable =
    sql"""
    CREATE TABLE Person (
      name TEXT NOT NULL PRIMARY KEY ,
      age  INTEGER
    )
    """.update.run

  private val createCoupleTable =
    sql"""
    CREATE TABLE Couple (
      him TEXT NOT NULL UNIQUE,
      her TEXT NOT NULL UNIQUE,
      FOREIGN KEY(him) REFERENCES Person(name),
      FOREIGN KEY(her) REFERENCES Person(name)
    )
    """.update.run

  private val createTaskTable =
    sql"""
    CREATE TABLE Task (
      tsk varchar(255) NOT NULL,
      emp varchar(255) NOT NULL,
      FOREIGN KEY (emp) REFERENCES Employee(emp)
    );
    """.update.run

  private val createEmployeeTable =
    sql"""
    CREATE TABLE Employee (
      emp varchar(255) PRIMARY KEY,
      dpt varchar(255) NOT NULL,
      FOREIGN KEY (dpt) REFERENCES Department(dpt)
    )
    """.update.run

  private val createDepartmentTable =
    sql"""
    CREATE TABLE Department (
      dpt varchar(255) PRIMARY KEY
    )
    """.update.run

  private def insertManyPerson(ps: List[Person]): ConnectionIO[Int] = {
    val sql = "insert into Person (name, age) values (?, ?)"
    Update[Person](sql).updateMany(ps)
  }

  private def insertManyCouple(ps: List[(String, String)]): ConnectionIO[Int] = {
    val sql = "insert into Couple (him, her) values (?, ?)"
    Update[(String, String)](sql).updateMany(ps)
  }

  private def insertManyDepartments(ps: List[Department]): ConnectionIO[Int] = {
    val sql = "insert into Department (dpt) values (?)"
    Update[String](sql).updateMany(ps.map(_.dpt))
  }

  private def insertManyEmployees(d: Department): ConnectionIO[Int] = {
    val sql = "insert into Employee (dpt, emp) values (?, ?)"
    Update[(String,String)](sql).updateMany(d.employees.map(e => (d.dpt, e.emp)))
  }

  private def insertManyTasks(e: Employee): ConnectionIO[Int] = {
    val sql = "insert into Task (emp, tsk) values (?, ?)"
    Update[(String,String)](sql).updateMany(e.tasks.map(t => (e.emp, t.tsk)))
  }

  private def insertCouples(couples: Couples): ConnectionIO[Int] = {
    val person = couples.flatMap(c => Iterator(c.him, c.her))
    val couplesToInsert = couples.map(c => (c.him.name, c.her.name))
    (insertManyPerson(person), insertManyCouple(couplesToInsert)).mapN(_ + _)
  }

  def prepareCoupleEnviroment(couples: Couples): ConnectionIO[Int] =
    (dropCoupleTable,
     dropPersonTable,
     createPersonTable,
     createCoupleTable,
     insertCouples(couples))
      .mapN(_ + _ + _ + _ + _)

  def prepareOrgEnviroment(departments: List[Department]): ConnectionIO[Int] = {
    val dropTables = (
      dropTable("Task"), 
      dropTable("Employee"), 
      dropTable("Department")).mapN(_ + _ + _)
    val createTables = (
      createDepartmentTable, 
      createEmployeeTable, 
      createTaskTable).mapN(_ + _ + _)
    val insertDepartmenst: ConnectionIO[Int] = 
      insertManyDepartments(departments)
    val insertEmployees: ConnectionIO[Int] = 
      departments.traverse(insertManyEmployees).map(_.sum)
    val insertTasks: ConnectionIO[Int] = 
      departments.flatMap(_.employees).traverse(insertManyTasks).map(_.sum)
    val insertAll = 
      (insertDepartmenst, insertEmployees, insertTasks).mapN(_ + _ + _)

    (dropTables, createTables, insertAll).mapN(_ + _ + _)
  }

  val xa = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    Transactor.fromDriverManager[IO](
      "org.sqlite.JDBC", "jdbc:sqlite:sample.db", "", "")
  }

  val transactor: Resource[IO, HikariTransactor[IO]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      te <- ExecutionContexts.cachedThreadPool[IO]    // our transaction EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.sqlite.JDBC",
        "jdbc:sqlite:sample.db",
        "", // username
        "", // password
        ce, // await connection here
        te  // execute JDBC operations here
      )
    } yield xa
  }
}

