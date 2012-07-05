package scales.xml

import strategies._

class XmlReaderTest29 extends junit.framework.TestCase {
  import junit.framework.Assert._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

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
