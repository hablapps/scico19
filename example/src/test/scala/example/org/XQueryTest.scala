package example
package org

import _root_.org.scalatest._
import optica._
import xquery._
import _root_.org.basex.core._
import _root_.org.basex.query._
import scala.collection.JavaConverters._
import Logic._

class XQueryTest extends FlatSpec with Matchers {

  def expertiseXQuery(u: String): XQuery =
    expertise[λ[x => XQuery], λ[x => XQuery]](u)

  "Optica" should "translate expertise into an XQuery expression" in {
    expertiseXQuery("abstract").toString shouldBe 
      """/xml/department[not(exists(employee[not(exists(task/tsk[. = "abstract"]))]))]/dpt"""
  }

  it should "work with a xml example" in {

    def process(query: String)(filePath: String):List[String] = {
      val xml = s"""doc('$filePath')""".stripMargin
      val str = s"""for $$x in $xml$query return data($$x)"""
      val context = new Context()
      new QueryProcessor(str, context).value().asScala.map(_.toString).toList
    }

    val query = expertiseXQuery("abstract").toString

    process(query)("example/src/test/resources/org.xml") shouldBe 
      List(""""Quality"""", """"Research"""")
  }
}

