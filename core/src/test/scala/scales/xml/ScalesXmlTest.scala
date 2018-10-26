package scales.xml

import org.specs2.mutable.Specification
import scales.utils._
import scales.xml.ScalesXml._
import scales.xml.xpath.{AttributePathText, XmlPathText}

import scala.xml.Source


object ScalesXmlTest extends Specification {

  "Testing the core functionality of scales-xml" should {
    "succeed" in {
      implicit lazy val v = Xml11
      val inputStream = this.getClass.getClassLoader.getResourceAsStream("test.xml")
      val doc = loadXml(Source.fromInputStream(inputStream))
      val path = top(doc)

      val value = path \* "table"  \* "title"
      value.one.map(XmlPathText.text(_)).headOption should beSome{ (txt: String) =>
        txt should_===("Some CALS Table")
      }

      val value1 = path \* "table"  \* "tgroup" \@ "cols"

      value1.one.map(AttributePathText.text(_)).headOption should beSome{ (txt: String) =>
        txt should_===("3")
      }

    }
  }

}
