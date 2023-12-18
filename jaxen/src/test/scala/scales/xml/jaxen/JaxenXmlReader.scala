package scales.xml.jaxen

class JaxenXmlReaderTest extends junit.framework.TestCase {
  import junit.framework.Assert._
  import scales.utils._
  import ScalesUtils._
  import scales.xml._
  import ScalesXml._

  /**
   * Thanks to Richard for this
   */ 
  def testYahoo : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = NuValidatorFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")

    val strPath = ScalesXPath("/ns:html/ns:body/ns:p[2]/ns:table[2]/ns:tbody/ns:tr/ns:td/ns:table/ns:tbody/ns:tr/ns:td[1]/ns:a/ns:font", ns.prefixed("ns"))

    assertEquals(9, strPath.evaluate(root).size)

    assertEquals(5, ScalesXPath("//ns:table", ns.prefixed("ns")).evaluate(root).size)
    assertEquals(1, ScalesXPath("//ns:table//ns:table", ns.prefixed("ns")).evaluate(root).size)
  }


  /**
   * Thanks to Richard for this
   */ 
  def testYahooNoNS : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = NuValidatorFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")

    val strPath = ScalesXPath("/html/body/p[2]/table[2]/tbody/tr/td/table/tbody/tr/td[1]/a/font").withNameConversion(ScalesXPath.localOnly)

    assertEquals(9, strPath.evaluate(root).size)

    assertEquals(5, ScalesXPath("//table").withNameConversion(ScalesXPath.localOnly).evaluate(root).size)
    assertEquals(1, ScalesXPath("//table//table").withNameConversion(ScalesXPath.localOnly).evaluate(root).size)
  }

  /**
   * Thanks to Richard for this
   */ 
  def testYahooNoNSTagSoup : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = TagSoupFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")
    
    // tag soup doesn't add tbodys
    val strPath = ScalesXPath("/html/body/p[2]/table[2]/tr/td/table/tr/td[1]/a/font").withNameConversion(ScalesXPath.localOnly)

    assertEquals(9, strPath.evaluate(root).size)

    assertEquals(5, ScalesXPath("//table").withNameConversion(ScalesXPath.localOnly).evaluate(root).size)
    assertEquals(1, ScalesXPath("//table//table").withNameConversion(ScalesXPath.localOnly).evaluate(root).size)
  }

}
