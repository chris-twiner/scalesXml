package scales.xml

import junit.framework.Assert._
import scales.utils._
import ScalesUtils._
import ScalesXml._
import org.xml.sax.XMLReader

object NuValidatorFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with DefaultSaxSupport {
  
  def create = {
    import nu.validator.htmlparser.{sax,common}
    import sax.HtmlParser
    import common.XmlViolationPolicy

    val reader = new HtmlParser
    reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
    // allow adds xmlns
    // reader.setXmlnsPolicy(XmlViolationPolicy.ALLOW)
    reader
  }      
  
}    

object TagSoupFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with DefaultSaxSupport {
  
  // doesn't support xml version retrieval
  override def getXmlVersion( reader : XMLReader ) : AnyRef =
    null

  def create = {
    import org.ccil.cowan.tagsoup.Parser
    val reader = new Parser
    // disable namespaces
    reader.setFeature(Parser.namespacesFeature, false)
    reader
  }      
  
}    

class XmlReaderTest extends junit.framework.TestCase {
  import junit.framework.Assert._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  /**
   * Thanks to Richard for this
   */ 
  def testYahoo : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = NuValidatorFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")

    val xpath = root.*(ns("html")).\*(ns("body")).\*(ns("p")).pos(2).\*(ns("table")).pos(2).\*(ns("tbody")).\*(ns("tr")).
    	\*(ns("td")).\*(ns("table")).\*(ns("tbody")).\*(ns("tr")).\*(ns("td")).pos(1).\*(ns("a")).\*(ns("font"))

    assertEquals(9, xpath.size)

    assertEquals(5, root.\\*(ns("table")).size)
    assertEquals(1, root.\\*(ns("table")).\\*(ns("table")).size)    
  }


  /**
   * Thanks to Richard for this
   */ 
  def testYahooNoNS : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = NuValidatorFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")

    val xpath = root.*:*("html").\*:*("body").\*:*("p").pos(2).\*:*("table").pos(2).\*:*("tbody").\*:*("tr").
    	\*:*("td").\*:*("table").\*:*("tbody").\*:*("tr").\*:*("td").pos(1).\*:*("a").\*:*("font")

    assertEquals(9, xpath.size)

    assertEquals(5, root.\\*:*("table").size)
    assertEquals(1, root.\\*:*("table").\\*:*("table").size)    
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
    val xpath = root.*("html").\*("body").\*("p").pos(2).\*("table").pos(2).\*("tr").
    	\*("td").\*("table").\*("tr").\*("td").pos(1).\*("a").\*("font")

    assertEquals(9, xpath.size)

    assertEquals(5, root.\\*("table").size)
    assertEquals(1, root.\\*("table").\\*("table").size)
  }

}

