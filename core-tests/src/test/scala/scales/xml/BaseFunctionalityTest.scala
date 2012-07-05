package scales.xml

import strategies._

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
  val prefixed = path \* "NoNamespace" \* hasLocalNameX("prefixed")
  
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
   

  val prefixedNoNS = path \*:* "NoNamespace" \*:* "prefixed"

  def testLocalNamePredicateNoNS = {
    assertTrue(prefixedNoNS.size == 1)
    assertEquals(prefixedPQN, pqName(prefixedNoNS.head))
  }

    
  def testNSAttributesDirectNoNS = {
    doAttrTest(prefixedNoNS \*:@("attr"))
  }
   

  def testLocalNamePredicateAttributesDirect = {
    doAttrTest(prefixed \@ hasLocalNameA("attr"))
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
    doAttrTest(prefixed.\.*@(hasLocalNameA("attr")))
  }

  def doExistsTest( f : (XPath[_]) => XPath[_]) = {
    val res = f(path \* ("NoNamespace"l))
    assertTrue("Did not find the attr in an element", res.size == 1)
    assertEquals("prefixed", localName(res.head))
  }

  def testExistsAttributes = {
    doExistsTest{ _ \*{ p : XmlPath => p.*@("urn:prefix" :: "attr") } }
  }
 
  import TestUtils._

  def positionAllKids : XmlPaths = path.\*(2).\*

  def testPositionAllKids = {
    val expected = List(dontRedeclareNoNS, dontRedeclareNoNS,
      shouldRedeclareDefaultNS, prefixedPQN)
    
    assertCompare(expected, positionAllKids)(pqName(_))
  }

  val dontRedeclaresX = path.\\.*(hasLocalNameX("DontRedeclare"))
  def dontRedeclares : XmlPaths = dontRedeclaresX

  def testDescendentLocalNamePredicate = {
    val expected = List("{urn:default}DontRedeclare",
      "{}DontRedeclare", "{}DontRedeclare");

    assertCompare(expected, dontRedeclares)(pqName(_))
  }

  def unions : XmlPaths = dontRedeclaresX | dontRedeclaresX | (dontRedeclaresX.\^)

  def testUnions = {
    val expected = List("{urn:default}Default", 
      "{urn:default}DontRedeclare",
      "{}NoNamespace",
      "{}DontRedeclare", "{}DontRedeclare");

    assertCompare(expected, unions)(
      pqName(_))
  }

  def parentsDuplicates : XmlPaths = dontRedeclaresX.\^

  // without the duplicate filter it would have three results
  def testParentsDuplicates = {
    val expected = List("{urn:default}Default",
      "{}NoNamespace");

    assertCompare(expected, parentsDuplicates )(pqName(_))
  }
    
  val allAttribsX = path.\\.*@

  def allAttribs : Iterable[AttributePath] = allAttribsX

  //  
  def testAllAttributes = {
    val expected = List("justHere:{urn:justHere}attr=only", "{}type=interesting", 
  			"ns1:{urn:prefix}attr=namespaced");
    assertCompare( expected, allAttribs ){ a => pqName(a) + "=" + value(a) }
  }

  def testChainedAttributes = {
    val expected = List("justHere:{urn:justHere}attr=only")
    assertCompare( expected, allAttribsX.*@(jh("attr")).*@(_.value == "only") ){
      a => pqName(a) + "=" + value(a) }
  }

  def testElementsWithAttributes = {
    val expected = List("{}NoNamespace")
    
    // the attrib list to boolean should get picked up
    assertCompare( expected, path.\\*("NoNamespace").*{
      implicit p => p.\@("type").*@(_.value == "interesting")}) { 
	pqName(_) }
  }

  def allElementsWithAttributes : XmlPaths = allAttribsX \^
  
  def testAllElementsWithAttributes = {
    val expected = List("{}NoNamespace", 
  			"ns1:{urn:prefix}prefixed");
    
    assertCompare( expected, allElementsWithAttributes ){ pqName(_) }
  }
  
  def testElementText = {
    val expected = List("prefixed text")
    assertCompare(expected, path.\\*("urn:prefix" :: "prefixed")) { text(_) }
  }

  def elementsPredicate : XmlPaths = path.\\*(_ === "prefixed text")

  def testElementPredicate = {
    val expected = List("ns1:{urn:prefix}prefixed")
    assertCompare(expected, elementsPredicate) { pqName(_) }
  }

  def normalizePredicate : XmlPaths = path.\\.*(normalizeSpace(_) == "start mix mode prefixed text end mix mode")

  def testNormalizePredicate = {
    val expected = List("{}NoNamespace")
    assertCompare(expected, normalizePredicate) { pqName(_) }
  }

  def testCData = {
    // we have two nodes before so 3 whitespaces.  Of course we actually should be getting path as well I think...
    val expected = List("should not have to be & < escaped @ all \"\"&")
    assertCompare(expected, path.\\.cdata) { value(_).trim }
  }

  def testComments = {
    val expected = List(" some comments are better than others ",
      " this wouldn't be one of them. ")
    assertCompare(expected, path.\+.comment) { value(_) }
  }

  /**
   * //a:ShouldRedeclare/../text()[5]/preceding-sibling::text()[1]
   *
  def testPreviousSimple = {
    val expected = List("start mix mode")
    assertCompare(expected, 
      path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4).\.preceding_sibling_::.text |> { x =>
	x.process( x.path.nodes.drop(3).take(1)) 
      }) { TextFunctions.value(_).trim }
    assertCompare(expected, 
      path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4).\.preceding_sibling_::.text.pos(3)
      ) { TextFunctions.value(_).trim }
    /*assertCompare(expected, 
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
    assertCompare(expected, 
      parentTextNodesRepeats(path)
      ) { value(_).trim }
  }

  def parentTextNodesMainRepeats : XmlPaths = path.\\.*("urn:default"::"ShouldRedeclare").\^.\+.text.pos(4)  |> { x => assertEquals("was not size 1", 1, x.path.nodes.size);x} 

  def testParentTextNodesMainRepeats = {
    val expected = one("start mix mode")
    assertCompare(expected, 
      parentTextNodesMainRepeats
      ) { value(_).trim }
  }

  def previousThenSibling : XmlPaths = path.\*("NoNamespace"l).\*.\.preceding_sibling_::.*.\.following_sibling_::.*

  def testPreviousThenSibling = {
    val expected = List("{}DontRedeclare",
			"{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed")
    assertCompare(expected, previousThenSibling )
    { pqName(_) }
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
    
    val paths = viewed(path).\\*(hasLocalNameX("DontRedeclare")).
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

    val paths = eager(path).\\.*(hasLocalNameX("DontRedeclare")).
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
    assertCompare(expected, path.\\.textOnly) { value(_).trim }
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
    assertCompare( expected, textP(path)){ value(_).trim }
  }

  def followingSiblings : XmlPaths = path.\\.*.\.following_sibling_::.*(2)

  // //*/following-sibling::*[2]
  def testFollowingSiblings = {
    val expected = List( "{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed");
    assertCompare(expected,
      followingSiblings
      ) { pqName(_) }
  }

  def precedingSiblings : XmlPaths  = path.\\.*.\.preceding_sibling_::.*(2)

  // //*/preceding-sibling::*[2]
  def testPrecedingSiblings = {
    val expected = List( "{}DontRedeclare",
			"{}DontRedeclare");
    assertCompare(expected,
      precedingSiblings
      ) { pqName(_) }
  }

  val loDontRedeclare = hasLocalNameX("DontRedeclare")

  def descendantSingleRoot : XmlPaths = path.\.descendant_::.*(loDontRedeclare).pos(1)

  // greedily swallows up, doesn't flatmap
  def testDescendantSingleRoot = {
    val expected = one("{urn:default}DontRedeclare")
    assertCompare(expected,
      descendantSingleRoot
      ) { pqName(_) }
  }

  def descendantMultipleRoots : XmlPaths = path.\\.descendant_::.*(loDontRedeclare).*(1)

  // two paths are opened up by \\ so it greedily works on two paths
  def testDescendantMultipleRoots = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      descendantMultipleRoots
      ) { pqName(_) }
  }

  def descendantMultipleRootsGt1 : XmlPaths = path.\\.descendant_::.*(loDontRedeclare).pos_>(1)
 
  def testDescendantMultipleRootsGt1 = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      descendantMultipleRootsGt1
      ) { pqName(_) }
  }
  
  def descendantMultipleRootsLt2 : XmlPaths = path.\\.descendant_::.*(loDontRedeclare).*.pos_<(2)

  def testDescendantMultipleRootsLt2 = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      descendantMultipleRootsLt2
      ) { pqName(_) }
  }

  def descendantText : XmlPaths = {
    path.\\.descendant_::.text.filter{ value(_).trim.length > 2}.pos_==(3)
  }

  // /descendant::text()[string-length(normalize-space(.)) > 2][3] "end mix mode"
  /** descendant doesn't just work on elems */ 
  def testDescendantText = {
    val expected = one("end mix mode")

    assertCompare(expected, 
      descendantText
      ) { value(_).trim }
  }

  val nestedXmlFile = resource(this, "/data/Nested.xml")
  val nestedXml = loadXml(nestedXmlFile)
  val nested = top(nestedXml)

  def descendantTextNested : XmlPaths = {
    nested.\.descendant_::.text.filter{ value(_).trim.length > 2}      
  }

  // this one doesn't test alot directly, its more of a sanity mechanism
  def testDescendantTextNested = {
    val expected = List("start mix mode","end mix mode")

    assertCompare(expected, 
      descendantTextNested
      ) { value(_).trim }
  } 

  def descendantSingleRootNested : XmlPaths = nested.\.descendant_::.*(loDontRedeclare).pos(1)

  // again sanity
  def testDescendantSingleRootNested = {
    val expected = one("{urn:default}DontRedeclare")
    assertCompare(expected,
      descendantSingleRootNested
      ) { pqName(_) }
  }

  def descendantMultipleRootsNested : XmlPaths = 
    nested.\\.descendant_::.*(loDontRedeclare).*(1)

  // two paths are opened up by \\ so it greedily works on two paths
  def testDescendantMultipleRootsNested = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      descendantMultipleRootsNested
      ) { pqName(_) }
  }
 
  def descendantMultipleRootsGt1Nested : XmlPaths = 
    nested.\\.descendant_::.*(loDontRedeclare).pos_>(1)

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
    assertCompare(expected,
      descendantMultipleRootsGt1Nested
      ) { pqName(_) }
  }

  def descendantMultipleRootsLt2Nested : XmlPaths =
    nested.\\.descendant_::.*(loDontRedeclare).*.pos_<(2)
  
  def testDescendantMultipleRootsLt2Nested = {
    val expected = List("{urn:default}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      descendantMultipleRootsLt2Nested
      ) { pqName(_) }
  }

  def lastEq : XmlPaths = path.\\*.last_==(4)

  // //*[last() = 4] 
  def testLastEq = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare",
			"{urn:default}ShouldRedeclare",
			"ns1:{urn:prefix}prefixed")
    assertCompare(expected,
      lastEq
      ) { pqName(_) }
  }

  def lastLt : XmlPaths = path.\\*("DontRedeclare"l).last_<(3)
  
  // //DontRedeclare[last() < 3]
  def testLastLt = {
    val expected = List("{}DontRedeclare",
			"{}DontRedeclare")
    assertCompare(expected,
      lastLt
      ) { pqName(_) }
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
    assertCompare(expected,
      lastGt
      ) { pqName(_) }
  }

  def posIsLast : XmlPaths = path.\*.\\*.pos_eq_last

  // /*//*[position() = last()]
  def testPosIsLast = {
    val expected = List("{}NoNamespace",
			"ns1:{urn:prefix}prefixed")
    assertCompare(expected,
      posIsLast
      ) { pqName(_) }
  }

  def posIsLastFromRoot : XmlPaths = path.\\*.pos_eq_last
  
  // //*[position() = last()]
  def testPosIsLastFromRoot = {
    val expected = List("{urn:default}Default",
			"{}NoNamespace",
			"ns1:{urn:prefix}prefixed")
    assertCompare(expected,
      posIsLastFromRoot
      ) { pqName(_) }
  }

  def textPosIsLast : XmlPaths = path.\\.text.pos_eq_last

  // /*//text()[position() = last()]
  def testTextPosIsLast = {
    val expected = List("prefixed text",
			"end mix mode",
			"") //last bit at the end of the doc after the commment

    assertCompare(expected,
      textPosIsLast
      ) { value(_).trim }
  }



  val ns = Namespace("test:uri")
  val nsa = Namespace("test:uri:attribs")
  val nsp = nsa.prefixed("pre")

  val builder = 
    ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	   "attr2" -> "val2",
		   nsp("attr3") -> "val3") /(
      ns("Child"),
      "Mixed Content",
      ns("Child2") /( ns("Subchild") ~> "text" )
    )

  val path2 = top(builder)
  
  def testEqs = {
    val res = path2 \* ns("Child2") * ( _.\*.===("text") )
    
    assertEquals("{test:uri}Child2", qualifiedName(res))

    val res2 = path2.\@.===("val1")
    
    assertEquals("{test:uri:attribs}attr1", qualifiedName(res2))
  }

  def testTypes = {
    val res = path2 \* ns("Child2") \* ns("Subchild")

    assertEquals("text", text(res))
    assertEquals("{test:uri}Subchild", qualifiedName(res))

    assertTrue(hasQName(res))

    val sub = res.\+.text

    assertEquals("text", text(sub))
    assertEquals("{}", qualifiedName(sub))

    try{
      name(sub) // should throw
      fail("Calling name on sub should not be possible")
    } catch {
      case t : Throwable => assertTrue("Was not our error but "+t.getMessage,
				     t.getMessage.startsWith("A Names instance"))
    }

    assertFalse(hasQName(sub))
  }

  def testAttributePathsTypes = {
    val res = path2 \@ nsp("attr3")

    assertTrue(hasQName(res))

    assertEquals("val3", string(res))
    assertEquals("{"+nsa.uri+"}attr3", qualifiedName(res))

    val res2 = path2 \@ nsp("notGonnaFindIt")

    assertEquals("", string(res2))
    assertFalse(hasQName(res2))

    try{
      name(res2) // should throw
      fail("Calling name on sub should not be possible")
    } catch {
      case t : Throwable => assertTrue("Was not our error but "+t.getMessage,
				     t.getMessage.startsWith("A Names instance"))
    }
  }

}
