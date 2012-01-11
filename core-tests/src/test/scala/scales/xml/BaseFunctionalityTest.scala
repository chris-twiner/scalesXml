package scales.xml

class BaseFunctionalityTest extends junit.framework.TestCase {
  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  import Functions._

  import strategies.TextNodeJoiner

  import BaseTestConstants._

  val xmlFile = resource(this, "/data/BaseXmlTest.xml")

  val testXml = loadXml(xmlFile)
  val path = top(testXml)

  def testElemConstruction : Unit = {
    val q = pre("local")
    val mu = Elem(q, pre, jh, defo, jh, pre)
    val eal = Elem(q, emptyAttributes, List(pre, jh, defo, jh, defo))
    val el = Elem(q, List(pre, jh, defo, jh, defo))
    val mua = Elem(q, emptyAttributes, pre, jh, defo, jh, pre)
    
    import scalaz._
    import Scalaz._
    
    mu.namespaces assert_=== eal.namespaces
    eal.namespaces assert_=== el.namespaces
    el.namespaces assert_=== mua.namespaces
    mua.namespaces assert_=== mu.namespaces
  }
// path.\*("NoNamespace").\*(localName_==("prefixed"))
  val prefixed = path \* "NoNamespace" \* localNameX_==("prefixed")

  def testLocalNamePredicate = {
    assertTrue(prefixed.size == 1)
    assertEquals(prefixedPQN, pqName(prefixed.head))
  }
/* */
  def doAttrTest(res: Iterable[AttributePath]) {
    assertTrue("Did not find attr", res.size == 1)
    val attrib = res.head.attribute
    assertEquals("ns1:{urn:prefix}attr", attrib.name.pqName)
    assertEquals("namespaced", attrib.value)
  }
    
  def testNSAttributesDirect = {
    doAttrTest(prefixed \@ ("urn:prefix"::"attr"))
  }
   
  def testLocalNamePredicateAttributesDirect = {
    doAttrTest(prefixed \@ localNameA_==("attr"))
  }

  def testNSAttributes = {
    doAttrTest(prefixed.\.*@("urn:prefix" :: "attr"))
  }

  def testPrefixedExactAttributes = {
    doAttrTest(prefixed.\.*@(Namespace("urn:prefix").prefixed("ns1", "attr")))
  }

  def testLocalOnlyFails = {
    assertTrue("Should not find an attribute, as this should only match for no namespace matches", prefixed.*@("attr").size == 0)
  }

  def testLocalNamePredicateAttributes = {
    doAttrTest(prefixed.\.*@(localNameA_==("attr")))
  }

  def doExistsTest( f : (XPath[_]) => XPath[_]) = {
    val res = f(path \* ("NoNamespace"l))
    assertTrue("Did not find the attr in an element", res.size == 1)
    assertEquals("prefixed", localName(res.head))
  }

  def testExistsAttributes = {
    doExistsTest{ _ \* (_.*@("urn:prefix" :: "attr") ) }
  }
 
  import TestUtils._

  def positionAllKids : XmlPaths = path.\*(2).\*

  def testPositionAllKids = {
    val expected = List(dontRedeclareNoNS, dontRedeclareNoNS,
      shouldRedeclareDefaultNS, prefixedPQN)
    
    compare(expected, positionAllKids)(pqName(_))
  }

  val dontRedeclaresX = path.\\.*(localNameX_==("DontRedeclare"))
  def dontRedeclares : XmlPaths = dontRedeclaresX

  def testDescendentLocalNamePredicate = {
    val expected = List("{urn:default}DontRedeclare",
      "{}DontRedeclare", "{}DontRedeclare");

    compare(expected, dontRedeclares)(pqName(_))
  }

  def unions : XmlPaths = dontRedeclaresX | dontRedeclaresX | (dontRedeclaresX.\^)

  def testUnions = {
    val expected = List("{urn:default}Default", 
      "{urn:default}DontRedeclare",
      "{}NoNamespace",
      "{}DontRedeclare", "{}DontRedeclare");

    compare(expected, unions)(
      pqName(_))
  }

  def parentsDuplicates : XmlPaths = dontRedeclaresX.\^

  // without the duplicate filter it would have three results
  def testParentsDuplicates = {
    val expected = List("{urn:default}Default",
      "{}NoNamespace");

    compare(expected, parentsDuplicates )(pqName(_))
  }
    
  val allAttribsX = path.\\.*@

  def allAttribs : Iterable[AttributePath] = allAttribsX

  //  
  def testAllAttributes = {
    val expected = List("justHere:{urn:justHere}attr=only", "{}type=interesting", 
  			"ns1:{urn:prefix}attr=namespaced");
    compare( expected, allAttribs ){ a => pqName(a) + "=" + value(a) }
  }

  def testChainedAttributes = {
    val expected = List("justHere:{urn:justHere}attr=only")
    compare( expected, allAttribsX.*@(jh("attr")).*@(_.value == "only") ){
      a => pqName(a) + "=" + value(a) }
  }

  def testElementsWithAttributes = {
    val expected = List("{}NoNamespace")
    
    // the attrib list to boolean should get picked up
    compare( expected, path.\\*("NoNamespace").*{
      implicit p => p.\@("type").*@(_.value == "interesting")}) { 
	pqName(_) }
  }

  def allElementsWithAttributes : XmlPaths = allAttribsX \^
  
  def testAllElementsWithAttributes = {
    val expected = List("{}NoNamespace", 
  			"ns1:{urn:prefix}prefixed");
    
    compare( expected, allElementsWithAttributes ){ pqName(_) }
  }
  
  def testElementText = {
    val expected = List("prefixed text")
    compare(expected, path.\\*("urn:prefix" :: "prefixed")) { text(_) }
  }

  def elementsPredicate : XmlPaths = path.\\*(_ === "prefixed text")

  def testElementPredicate = {
    val expected = List("ns1:{urn:prefix}prefixed")
    compare(expected, elementsPredicate) { pqName(_) }
  }

  def normalizePredicate : XmlPaths = path.\\.*(normalizeSpace(_) == "start mix mode prefixed text end mix mode")

  def testNormalizePredicate = {
    val expected = List("{}NoNamespace")
    compare(expected, normalizePredicate) { pqName(_) }
  }

  def testCData = {
    // we have two nodes before so 3 whitespaces.  Of course we actually should be getting path as well I think...
    val expected = List("should not have to be & < escaped @ all \"\"&")
    compare(expected, path.\\.cdata) { value(_).trim }
  }

  def testComments = {
    val expected = List(" some comments are better than others ",
      " this wouldn't be one of them. ")
    compare(expected, path.\+.comment) { value(_) }
  }

  /**
   * //a:ShouldRedeclare/../text()[5]/preceding-sibling::text()[1]
   *
  def testPreviousSimple = {
    val expected = List("start mix mode")
    compare(expected, 
      path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4).\.preceding_sibling_::.text |> { x =>
	x.process( x.path.nodes.drop(3).take(1)) 
      }) { TextFunctions.value(_).trim }
    compare(expected, 
      path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4).\.preceding_sibling_::.text.pos(3)
      ) { TextFunctions.value(_).trim }
    /*compare(expected, 
      path.\\.*("urn:default"::"ShouldRedeclare").\^.\.text.pos(4).\.preceding_sibling_::.text.\.previous
      ) { TextFunctions.value(_).trim }*/
  } */

  def parentTextNodesRepeats(path : XmlPath) : XmlPaths = path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4) |> { x => assertEquals("was not size 2", 2, x.path.nodes.size);x}

  /**
   * //a:ShouldRedeclare/../text()[4]     
   */ 
  def testParentTextNodesRepeats = { // /preceding-sibling::text()[1]
    val testXml = loadXml(resource(this, "/data/BaseXmlTestRepeats.xml"))
    val path = top(testXml)
    
    val expected = List("start mix mode","start mix mode")
    compare(expected, 
      parentTextNodesRepeats(path)
      ) { TextFunctions.value(_).trim }
  }

  def parentTextNodesMainRepeats : XmlPaths = path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4)  |> { x => assertEquals("was not size 1", 1, x.path.nodes.size);x} 

  def testParentTextNodesMainRepeats = {
    val expected = one("start mix mode")
    compare(expected, 
      parentTextNodesMainRepeats
      ) { TextFunctions.value(_).trim }
  }

  def previousThenSibling : XmlPaths = path.\*("NoNamespace"l).\*.\.preceding_sibling_::.*.\.following_sibling_::.*

  def testPreviousThenSibling = {
    val expected = List("{}DontRedeclare",
			"{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed")
    compare(expected, previousThenSibling )
    { Elements.Functions.pqName(_) }
  }

  val utf8 = {
    val reader = new java.io.InputStreamReader(resource(this, "/data/BaseFunctionalityTestEntityTest.utf8").openStream, "UTF-8")
    val buffered = new java.io.BufferedReader(reader)
    val rutf8 = buffered.readLine()
    buffered.close()
    rutf8
  } 

  def testLazyViewed : Unit = {
    var res : List[XmlPath] = Nil
    import Elements.localName

    val paths = viewed(path).\\.*(localName("DontRedeclare")).
      filter{
	path => 
	  res = path :: res
	  true
      }
 
    assertEquals(0, res.size)

    val bits = lazyRaw(paths)
    bits.head
    assertEquals(1, res.size)

    bits.head // lazy but based on stream, implementation details ftw?
    assertEquals(1, res.size)
  }

  def testEager : Unit = {
    var res : List[XmlPath] = Nil
    import Elements.localName

    val paths = eager(path).\\.*(localName("DontRedeclare")).
      filter{
	path => 
	  res = path :: res
	  true
      }
    
    assertEquals(3, res.size)
  }

  /** Can't do in normal xpath, as it needs to filter out the cdatas */
  def testTextOnly : Unit = {
    val tnj = new TextNodeJoiner[QNameToken] with QNameTokenF {} 
    // or indeed mixed in.
    //new MutableVectorLikeStrategy with ElemQNameOptimisationT with TextNodeJoiner {}
    // join up the text nodes
    val testXml2 = loadXml(xmlFile, strategy = tnj)
    val path = top(testXml2)

    // we have two nodes before so 3 whitespaces.  Of course we actually should be getting path as well I think...
    val expected = List("", "", "", "", "", "start mix mode", "prefixed text", "end mix mode",
      "\"\n\t" + utf8,
      "mix it up some more", "", "")
    compare(expected, path.\\.textOnly) { TextFunctions.value(_).trim }
  }

  def textP( path : XmlPath ) : XmlPaths = path.\\.text

  def testText = {
    // we have two nodes before so 3 whitespaces.  Of course we actually should be getting path as well I think...

    val tnj = new TextNodeJoiner[QNameToken] with QNameTokenF {} 
    val testXml2 = loadXml(xmlFile, strategy = tnj)
    val path = top(testXml2)

    val expected = List("", "", "", "", "", "start mix mode", "prefixed text", "end mix mode",
      "\"\n\t" + utf8,
      "should not have to be & < escaped @ all \"\"&",
      "mix it up some more",
      "", "")
    // we, like Jaxen work correctly, Saxon can't see the n-3 and 4
    compare( expected, textP(path)){ TextFunctions.value(_).trim }
  }

  def followingSiblings : XmlPaths = path.\\.*.\.following_sibling_::.*(2)

  // //*/following-sibling::*[2]
  def testFollowingSiblings = {
    val expected = List( "{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed");
    compare(expected,
      followingSiblings
      ) { Elements.Functions.pqName(_) }
  }

  def precedingSiblings : XmlPaths  = path.\\.*.\.preceding_sibling_::.*(2)

  // //*/preceding-sibling::*[2]
  def testPrecedingSiblings = {
    val expected = List( "{}DontRedeclare",
			"{}DontRedeclare");
    compare(expected,
      precedingSiblings
      ) { Elements.Functions.pqName(_) }
  }

  def descendantSingleRoot : XmlPaths = path.\.descendant_::.*(Elements.localName("DontRedeclare")).pos(1)

  // greedily swallows up, doesn't flatmap
  def testDescendantSingleRoot = {
    val expected = one("{urn:default}DontRedeclare")
    compare(expected,
      descendantSingleRoot
      ) { Elements.Functions.pqName(_) }
  }

  def descendantMultipleRoots : XmlPaths = path.\\.descendant_::.*(Elements.localName("DontRedeclare")).*(1)

  // two paths are opened up by \\ so it greedily works on two paths
  def testDescendantMultipleRoots = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRoots
      ) { Elements.Functions.pqName(_) }
  }

  def descendantMultipleRootsGt1 : XmlPaths = path.\\.descendant_::.*(Elements.localName("DontRedeclare")).pos_>(1)
 
  def testDescendantMultipleRootsGt1 = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRootsGt1
      ) { Elements.Functions.pqName(_) }
  }
  
  def descendantMultipleRootsLt2 : XmlPaths = path.\\.descendant_::.*(Elements.localName("DontRedeclare")).*.pos_<(2)

  def testDescendantMultipleRootsLt2 = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRootsLt2
      ) { Elements.Functions.pqName(_) }
  }

  def descendantText : XmlPaths = {
    import TextFunctions.{value => tvalue}
    path.\\.descendant_::.text.filter{ implicit p => tvalue.trim.length > 2}.pos_==(3)
  }

  // /descendant::text()[string-length(normalize-space(.)) > 2][3] "end mix mode"
  /** descendant doesn't just work on elems */ 
  def testDescendantText = {
    import TextFunctions.{value => tvalue}
    val expected = one("end mix mode")

    compare(expected, 
      descendantText
      ) { tvalue(_).trim }
  } 

  val nestedXmlFile = resource(this, "/data/Nested.xml")
  val nestedXml = loadXml(nestedXmlFile)
  val nested = top(nestedXml)

  def descendantTextNested : XmlPaths = {
    import TextFunctions.{value => tvalue}
    nested.\.descendant_::.text.filter{ implicit p => tvalue.trim.length > 2}      
  }

  // this one doesn't test alot directly, its more of a sanity mechanism
  def testDescendantTextNested = {
    import TextFunctions.{value => tvalue}
    val expected = List("start mix mode","end mix mode")

    compare(expected, 
      descendantTextNested
      ) { tvalue(_).trim }
  } 

  def descendantSingleRootNested : XmlPaths = nested.\.descendant_::.*(Elements.localName("DontRedeclare")).pos(1)

  // again sanity
  def testDescendantSingleRootNested = {
    val expected = one("{urn:default}DontRedeclare")
    compare(expected,
      descendantSingleRootNested
      ) { Elements.Functions.pqName(_) }
  }

  def descendantMultipleRootsNested : XmlPaths = 
    nested.\\.descendant_::.*(Elements.localName("DontRedeclare")).*(1)

  // two paths are opened up by \\ so it greedily works on two paths
  def testDescendantMultipleRootsNested = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRootsNested
      ) { Elements.Functions.pqName(_) }
  }
 
  def descendantMultipleRootsGt1Nested : XmlPaths = 
    nested.\\.descendant_::.*(Elements.localName("DontRedeclare")).pos_>(1)

  def testDescendantMultipleRootsGt1Nested = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRootsGt1Nested
      ) { Elements.Functions.pqName(_) }
  }

  def descendantMultipleRootsLt2Nested : XmlPaths =
    nested.\\.descendant_::.*(Elements.localName("DontRedeclare")).*.pos_<(2)
  
  def testDescendantMultipleRootsLt2Nested = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      descendantMultipleRootsLt2Nested
      ) { Elements.Functions.pqName(_) }
  }

  def lastEq : XmlPaths = path.\\*.last_==(4)

  // //*[last() = 4] 
  def testLastEq = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare",
			"{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed")
    compare(expected,
      lastEq
      ) { Elements.Functions.pqName(_) }
  }

  def lastLt : XmlPaths = path.\\*("DontRedeclare"l).last_<(3)
  
  // //DontRedeclare[last() < 3]
  def testLastLt = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare")
    compare(expected,
      lastLt
      ) { Elements.Functions.pqName(_) }
  }

  def lastGt : XmlPaths = path.\\*.last_>(1)

  // //*[last() > 1]
  def testLastGt = {
    val expected = List("{urn:default}DontRedeclare",
			"{}NoNamespace",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed")
    compare(expected,
      lastGt
      ) { Elements.Functions.pqName(_) }
  }

  def posIsLast : XmlPaths = path.\*.\\*.pos_eq_last

  // /*//*[position() = last()]
  def testPosIsLast = {
    val expected = List("{}NoNamespace",
			"ns1:{urn:prefix}prefixed")
    compare(expected,
      posIsLast
      ) { Elements.Functions.pqName(_) }
  }

  def posIsLastFromRoot : XmlPaths = path.\\*.pos_eq_last
  
  // //*[position() = last()]
  def testPosIsLastFromRoot = {
    val expected = List("{urn:default}Default",
			"{}NoNamespace",
			"ns1:{urn:prefix}prefixed")
    compare(expected,
      posIsLastFromRoot
      ) { Elements.Functions.pqName(_) }
  }

  def textPosIsLast : XmlPaths = path.\\.text.pos_eq_last

  // /*//text()[position() = last()]
  def testTextPosIsLast = {
    val expected = List("prefixed text",
			"end mix mode",
			"") //last bit at the end of the doc after the commment

    compare(expected,
      textPosIsLast
      ) { TextFunctions.value(_).trim }
  }
}
