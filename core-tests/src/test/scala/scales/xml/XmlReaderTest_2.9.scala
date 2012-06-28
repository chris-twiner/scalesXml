package scales.xml

import strategies._

class XmlReaderTest extends junit.framework.TestCase {
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

  object TagSoupFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with SaxSupport {
    

    def setLexicalHandler[T <: OptimisationToken]( reader : XMLReader, handler : Handler[T] ) : Unit = {
      reader.setProperty("http://xml.org/sax/properties/lexical-handler", handler)    
    }

    // doesn't support xml version retrieval
    def getXmlVersion( reader : XMLReader ) : AnyRef =
      null

    def create = {
      import org.ccil.cowan.tagsoup.Parser
      val reader = new Parser
      reader.setFeature(Parser.namespacesFeature, false)
      // allow adds xmlns
      // reader.setXmlnsPolicy(XmlViolationPolicy.ALLOW)
      reader
    }      
    
  }    

  /**
   * Thanks to Richard for this
   */ 
  def testYahoo : Unit = {
    val urlListing = resource(this, "/data/yahoo.htm")

    val doc = loadXmlReader(urlListing, parsers = NuValidatorFactoryPool)
    val root = top(doc)

    val ns = Namespace("http://www.w3.org/1999/xhtml")

    val xpath = root.*(ns("html")).\*(ns("body")).\*(ns("p")).pos(2).\*(ns("table")).pos(2).\*(ns("tbody")).\*(ns("tr")).
       \*(ns("td")).pos(1).\*(ns("a")).\*(ns("font"))
      //ScalesXPath("/html/body/p[2]/table[2]/tbody/tr/td/table/tbody/tr/td[1]/a/font")

    //assertEquals(println(root.\\*.size)             // 477 :: this one works! :)

    //val ns = Namespace("http://www.w3.org/1999/xhtml")
    assertEquals(5, root.\\*(ns("table")).size)         //   5 :: fails :(
    assertEquals(1, root.\\*(ns("table")).\\*(ns("table")).size)
//    println(ScalesXPath("//table//table").evaluate(root).size)  //   1 :: fails :(

    println(xpath.size)                          //   9 :: fails :(
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
