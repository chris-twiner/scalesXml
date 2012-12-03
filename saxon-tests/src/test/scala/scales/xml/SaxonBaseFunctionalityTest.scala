package scales.xml

class SaxonBaseFunctionalityTest extends test.BaseFunctionalityTest {

  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._
  import TestUtils._

  import Functions._

  val dom = {
    val dbf = javax.xml.parsers.DocumentBuilderFactory.newInstance
    dbf.setNamespaceAware(true)
    dbf.setIgnoringComments(false)
    dbf.setIgnoringElementContentWhitespace(false)
    val db = dbf.newDocumentBuilder
    db.parse(xmlFile)
  }

  def qname(node: org.w3c.dom.Node) = {
    val ns = node.getNamespaceURI
    "{" + (if (ns eq null) "" else ns) + "}" + node.getLocalName
  }

  def testElementTextSaxon = {
    val expected = List("prefixed text")
    assertCompare(expected, path.\\.*("urn:prefix" :: "prefixed")) { text(_) }
    assertCompare(expected, nodeSet("//*[local-name()='prefixed' and namespace-uri()='urn:prefix']", dom)) { _.getTextContent.trim }
  }

  def testElementPredicateSaxon = {
    val expected = List("ns1:{urn:prefix}prefixed")
    assertCompare(expected, path.\\.*(_ === "prefixed text")) { pqName(_) }
    assertCompare(expected, nodeSet("//*[. = 'prefixed text']", dom)) { x => x.getPrefix + ":" + qname(x) }
  }

  def testNormalizePredicateSaxon = {
    val expected = List("{}NoNamespace")
    assertCompare(expected, path.\\.*(normalizeSpace(_) == "start mix mode prefixed text end mix mode")) { pqName(_) }
    assertCompare(expected, nodeSet("//*[normalize-space(.) = 'start mix mode prefixed text end mix mode']", dom)) { qname(_) }
    // /*(fn:normalize-space(.) = 'start mix mode prefixed text end mix mode'
  }

  def testTextSaxon = {
    // we have two nodes before so 3 whitespaces.  Of course we actually should be getting path as well I think...
    val expected = List("", "", "", "", "", "start mix mode", "prefixed text", "end mix mode",
      "\"\n\t" + utf8,
      //"should not have to be & < escaped @ all \"\"&",
      //"mix it up some more",
      "", "")
    // Saxon can't see the two above nodes, which sucks, we and Jaxen can
    assertCompare(expected, nodeSet("//text()", 
			      xmlFile
//			      dom //  both stream and dom are incorrect :<
			    )) { _.getTextContent.trim }
  }

  val xfact = new net.sf.saxon.xpath.XPathFactoryImpl();

  type DomNode = org.w3c.dom.Node

  def xstring(path: String, in: AnyRef/*org.w3c.dom.Document*/) = {
    val xpath = xfact.newXPath

    val texts = xpath.compile(path)

    texts.evaluate(
      in,
      javax.xml.xpath.XPathConstants.STRING).asInstanceOf[java.lang.String];
  }

  def node(path: String, in: AnyRef) = {
    val xpath = xfact.newXPath

    val texts = xpath.compile(path)

    texts.evaluate(
      dom,
      javax.xml.xpath.XPathConstants.NODE).asInstanceOf[org.w3c.dom.Node];
  }

  def nodeSet(path: String, in: AnyRef) = {
    val xpath = xfact.newXPath

    val texts = xpath.compile(path)

    val nodeList = texts.evaluate(
      dom,
      javax.xml.xpath.XPathConstants.NODESET).asInstanceOf[org.w3c.dom.NodeList];

    new Iterable[DomNode] {
      def iterator() = new Iterator[DomNode]() {
        var index = 0
        override val size = nodeList.getLength

        def hasNext = index < size
        def next(): DomNode = {
          val res = nodeList.item(index)
          index += 1;
          res
        }
      }

    }
  }

}
