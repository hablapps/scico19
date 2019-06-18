package example
package couple

import _root_.org.basex.core._
import _root_.org.basex.query._
import _root_.org.scalatest._
import optica._
import xquery._
import scala.collection.JavaConverters._
import Logic._

class XQueryTest extends FlatSpec with Matchers {

  val differencesXQuery: XQuery =
    differences[λ[x => XQuery], λ[x => XQuery]]

  "Optica" should "translate differences into an XQuery expression" in {
    differencesXQuery.toString shouldBe 
      "/xml/couple[her/age > him/age]/<tuple><fst>{her/name}</fst><snd>{her/age - him/age}</snd></tuple>"
  }

  it should "work with a xml example" in {

    def process(query: String)(filePath: String): List[String] = {
      val xml = s"""doc('$filePath')""".stripMargin
      val str = s"""for $$x in $xml$query return data($$x)"""
      val context = new Context()
      new QueryProcessor(str, context).value().asScala.map(_.toString).toList
    }

    val query = differencesXQuery.toString

    process(query)("example/src/test/resources/couple.xml") shouldBe 
      List(""""Alex5"""",""""Cora2"""")
  }
}

