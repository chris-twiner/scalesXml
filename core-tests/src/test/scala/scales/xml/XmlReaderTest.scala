package scales.xml

import strategies._

class XmlReaderTest extends junit.framework.TestCase {
  import junit.framework.Assert._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._
  import org.xml.sax.XMLReader

  object NuValidatorFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] { pool =>
    
    def create = {
      import nu.validator.htmlparser.{sax,common}
      import sax.HtmlParser
      import common.XmlViolationPolicy

      val reader = new HtmlParser
      reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
      reader.setXmlnsPolicy(XmlViolationPolicy.ALLOW)
      reader
    }      
    
  }    

  def testNuXmlReader : Unit = {
    val xmlFile = resource(this, "/data/html.xml")
    val nuxml = loadXmlReader(xmlFile, parsers = NuValidatorFactoryPool)

    // nu makes them all lower case
    val xml = loadXml(resource(this, "/data/htmlLike.xml"))

    import scalaz._
    import Scalaz._
    
    assertTrue( xml === nuxml )
  }

}
