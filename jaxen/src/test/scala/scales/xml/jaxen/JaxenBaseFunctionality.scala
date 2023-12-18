package scales.xml.jaxen

import scales.utils._
import ScalesUtils._
import scales.xml._
import ScalesXml._

import Functions._

class JaxenBaseFunctionalityTest extends test.BaseFunctionalityTest {

  import junit.framework.Assert._

  import scala.collection.JavaConversions._

  import BaseTestConstants._

  def jaxen(str : String) =
    ScalesXPath(str, Map(
      "pre" -> "urn:prefix",
      "jh" -> "urn:justHere",
      "def" -> "urn:default"
    ))


  def testConstruction : Unit = {
    val ma = jaxen("//*")

    val mu = ScalesXPath("//*", pre, jh, defo, jh, pre)
    val i = ScalesXPath("//*", List(pre, jh, defo, jh, defo))

    import scalaz._
    import Scalaz._

    ma.nsMap assert_=== mu.nsMap
    mu.nsMap assert_=== i.nsMap
    i.nsMap assert_=== ma.nsMap
  }

  val jprefixedPath = "/def:Default/NoNamespace/*[local-name(.) = 'prefixed']"

  def fromPathX(str : String, path : XmlPath) : Iterable[XmlPath] = 
    jaxen(str).xmlPaths(path)

  def fromPathA(str : String, path : XmlPath) : Iterable[AttributePath] = 
    jaxen(str).attributePaths(path)

  def fromPathX(str : String) : Iterable[XmlPath] = 
    fromPathX(str, path)

  def fromPathA(str : String) : Iterable[AttributePath] = 
    fromPathA(str, path)

  def get[T](str : String) = 
    jaxen(str).get[T](path)

  def testLocalNamePredicateJaxen = {
    val res = fromPathX(jprefixedPath)
    assertTrue(res.size == 1)
    assertEquals(prefixedPQN, pqName(res.head))
  }

  def testNSAttributesJaxen = {
    val res = fromPathA(jprefixedPath + "/@pre:attr")
    doAttrTest(res)
  }

  val jprefixedPathNoNS = "/Default/NoNamespace/prefixed"

  def testNoNSLocalNamePredicateJaxen = {
    val res = jaxen(jprefixedPathNoNS).withNameConversion(ScalesXPath.localOnly).xmlPaths(path)
    assertTrue(res.size == 1)
    assertEquals(prefixedPQN, pqName(res.head))
  }

  def testNoNSNSAttributesJaxen = {
    val res = jaxen(jprefixedPathNoNS + "/@attr").withNameConversion(ScalesXPath.localOnly).attributePaths(path)
    doAttrTest(res)
  }

  def testLocalOnlyFailsJaxen = {
    val res = fromPathA(jprefixedPath + "/@attr")
    assertTrue("Should not find an attribute, as this should only match for no namespace matches", res.size == 0)
  }

  def testLocalNamePredicateAttributesJaxen = {
    val res = fromPathA(jprefixedPath + "/@*[local-name(.) = 'attr']")
    doAttrTest(res)
  }

  def testExistsAttributesJaxen = {
    val res = fromPathX("/*/NoNamespace/*[ @pre:attr ]")
    assertTrue("Did not find the attr in an element", res.size == 1)
    assertEquals("prefixed", localName(res.head))
  }
 
  import TestUtils._

  override def positionAllKids = fromPathX("/*/*[2]/*")

  val jdontRedeclares = "/*//*[local-name(.) = 'DontRedeclare']"
  override def dontRedeclares = fromPathX(jdontRedeclares)

  override def unions = 
    fromPathX(jdontRedeclares +" | "+ jdontRedeclares +" | "+
	      jdontRedeclares +"/..")

  override def parentsDuplicates = fromPathX(jdontRedeclares + "/..")
    
  val jAllAttribs = "//@*"

  override def allAttribs = fromPathA(jAllAttribs)

  override def allElementsWithAttributes = fromPathX(jAllAttribs + "/..")
  
  def testElementTextJaxen : Unit =
    assertEquals("prefixed text", get("string(//pre:prefixed)"))

  override def elementsPredicate = fromPathX("//*[. = 'prefixed text']")

  override def normalizePredicate = 
    fromPathX("//*[normalize-space(string()) = 'start mix mode prefixed text end mix mode']")
 
  override def parentTextNodesRepeats(path : XmlPath) = 
    fromPathX("//def:ShouldRedeclare/../text()[4]", path)

  override def parentTextNodesMainRepeats = 
    fromPathX("//def:ShouldRedeclare/../text()[4]")

  override def previousThenSibling = 
    fromPathX("/*/NoNamespace/*/preceding-sibling::*/following-sibling::*")

  override def textP( path : XmlPath ) = fromPathX("//text()",path)

  def testNonRootContext = {
    val sub = path \*(2)
    val expected = one( "DontRedeclare"l : QName)
    assertCompare(expected, fromPathX("./*[2]", sub.one.head)) {elem(_).name}
  }

  def testNonRootContextDoc = {
    val sub = path \*(2)
    val expected = one( "NoNamespace"l : QName)
    assertCompare(expected, fromPathX("./*[1]/..", sub.one.head)) {elem(_).name}
  }

  def testNonRootContextDocsParentShouldBeEmpty = {
    val sub = path \*(2)
    val res = fromPathX("./*[1]/../../../..", sub.one.head)
    assertTrue("Should have been empty, doc doesn't have a root: " + res, res.isEmpty)
  }

  override def followingSiblings = fromPathX("//*/following-sibling::*[2]")

  override def precedingSiblings = fromPathX("//*/preceding-sibling::*[2]")

  override def descendantSingleRoot = fromPathX("/descendant::*[local-name() = 'DontRedeclare'][1]")

  override def descendantMultipleRoots = fromPathX("//descendant::*[local-name() = 'DontRedeclare'][1]")

  override def descendantMultipleRootsGt1 = 
    fromPathX("//descendant::*[local-name() = 'DontRedeclare'][position() > 1]")
 
  override def descendantMultipleRootsLt2 = 
    fromPathX("//descendant::*[local-name() = 'DontRedeclare'][position() < 2]")
 
  override def descendantText = 
    fromPathX("/descendant::text()[string-length(normalize-space(.)) > 2][3]")

  override def descendantTextNested = 
    fromPathX("/descendant::text()[string-length(normalize-space(.)) > 2]", nested)

  override def descendantSingleRootNested = 
    fromPathX("/descendant::*[local-name() = 'DontRedeclare'][1]", nested)

  override def descendantMultipleRootsNested = 
    fromPathX("//descendant::*[local-name() = 'DontRedeclare'][1]", nested)
    
  override def descendantMultipleRootsGt1Nested = 
    fromPathX("//descendant::*[local-name() = 'DontRedeclare'][position() > 1]", nested)
  
  override def descendantMultipleRootsLt2Nested = 
    fromPathX("//descendant::*[local-name() = 'DontRedeclare'][position() < 2]", nested)

  override def lastEq = fromPathX("//*[last()=4]")
  
  override def lastLt = fromPathX("//DontRedeclare[last() < 3]")

  override def lastGt = fromPathX("//*[last() > 1]")

  override def posIsLast = fromPathX("/*//*[position() = last()]")
  
  override def posIsLastFromRoot = fromPathX("//*[position() = last()]")

  override def textPosIsLast = fromPathX("/*//text()[position() = last()]")

  def testDescendantAll =
    doTestAll("descendant", _.descendant_::)
  
  def testAncestorAll =
    doTestAll("ancestor", _.ancestor_::)
  
  def testAncestorOrSelfAll =
    doTestAll("ancestor-or-self", _.ancestor_or_self_::)
  
  def testDescendantOrSelfAll =
    doTestAll("descendant-or-self", _.descendant_or_self_::)

  def testPrecedingAll = 
    doTestAll("preceding", _.preceding_::)

  def testFollowingAll =
    doTestAll("following", _.following_::)

  def doTestAll(str : String, p : scales.xml.XPath[List[XmlPath]] => scales.xml.XPath[List[XmlPath]]) = {
    val expected = fromPathX("//"+str+"::*", nested).map(pqName(_))
  //  println("expected")
    //expected foreach println

    assertCompare( expected ,
      //p(nested .\\).
	nested.\\.|>(p).*.map(pqName(_)) 
      ) {identity}
  }

  // #22
  def testRelativeAndImplicitChildNode : Unit = { 
    
    def xpathExpr(expr: String) =
      ScalesXPath(expr).withNameConversion(ScalesXPath.localOnly)

    val xmlText = """
    <level1>
      <level2 attrib="value">
	<level3>contents of level 3</level3>
	<shouldHideChildren>
	  <level3>should not see</level3>
	</shouldHideChildren>
      </level2>
    </level1>      
    """

    val doc = loadXmlReader(new java.io.StringReader(xmlText), parsers = scales.xml.impl.NoVersionXmlReaderFactoryPool)
    val root = top(doc)

    val level2 = xpathExpr("/level1/level2").xmlPaths(root).head
    //   println("found level2: " + string(level2))

    val relative = xpathExpr("./level3").xmlPaths(level2).head
    assertEquals("relative should be level3", "level3", localName(relative))

    val current = xpathExpr(".").xmlPaths(level2).head
    assertEquals("current should be level2", "level2", localName(current))

    val parent = xpathExpr("..").xmlPaths(level2).head
    assertEquals("parent should be level1", "level1", localName(parent))

    val parent2 = xpathExpr("./..").xmlPaths(level2).head
    assertEquals("parent2 should be level1", "level1", localName(parent2))

    // not sure where this implicit child context comes from specwise but it seems to be standard behaviour for jaxen
    val directName = xpathExpr("level3").xmlPaths(level2).head
    assertEquals("directname should be level3", "level3", localName(directName))

    val directText = xpathExpr("text()").xmlPaths(relative).head
    assertTrue("should be text", directText.isItem && (directText.item.isInstanceOf[Text] ))
    assertEquals("text should be - contents of level 3", "contents of level 3", text(directText))

    val attrib = xpathExpr("./@attrib").attributePaths(level2).head
    assertEquals("should have been value", "value", text(attrib))
  }

}
