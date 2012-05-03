package scales.xml.equalsTest // different to keep default xml._ out

import scales.xml._
import ScalesXml._
import Functions._

import scales.xml.equals._

class EqualsNormalImportsTest extends junit.framework.TestCase {
  // these guys import a resource
  import scalaz._
  import Scalaz._

  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._

  val xmlFile = loadXml(scales.utils.resource(this, "/data/BaseXmlTest.xml"))
  val xml = xmlFile.rootElem
  val xmlFile2 = loadXml(scales.utils.resource(this, "/data/BaseXmlTest.xml"))
  val xml2 = xmlFile2.rootElem

  val ns = Namespace("urn:default")

  // attributes are here
  val nons = ( top(xml) \* 2 ).head
  val nons2 = ( top(xml2) \* 2 ).head

  val n = Namespace("uri:prefix")
  val no = Namespace("uri:prefixed")
  val p = n.prefixed("p")
  val po = no.prefixed("po")
  
  val qn = po("elem")
  import BasicPaths._

  // make sure its passed through where it should be
  val DummyPath : BasicPath = List(("n"l, Map()))
  val DummyContext = ComparisonContext(DummyPath)

  def testItems : Unit = {

    val t1 = Text("fred")
    val t2 = Text("fred")

    assertTrue("t1 == t2", t1 == t2)
    assertTrue("t1 === t2", t1 === t2)

    assertTrue("t1 compare t2", compare(DummyContext, t1, t2).isEmpty )
    
    val t3 = Text("freed")

    assertFalse("t1 == t3", t1 == t3)
    assertFalse("t1 === t3", t1 === t3)

    val t1and3c = defaultXmlItemComparison.compare(true, DummyContext, t1, t3)
    assertFalse("t1and3c.isEmpty", t1and3c.isEmpty )
    
    val Some((ItemDifference(t13cl, t13cr), DummyContext)) = t1and3c 
    assertTrue("t13cl", t13cl eq t1)
    assertTrue("t13cr", t13cr eq t3)
    
    val t1and3nc = defaultXmlItemComparison.compare(false, DummyContext, t1, t3)
    assertFalse("t1and3nc.isEmpty", t1and3nc.isEmpty )
    
    val Some((SomeDifference(t13ncl, t13ncr), tpn)) = t1and3nc 
    assertTrue("t13ncl", t13ncl eq null)
    assertTrue("t13ncr", t13ncr eq null)
    
    val cd1 = CData("fred")
    
    val ct = defaultXmlItemComparison.compare(true, DummyContext, cd1, t1)
    assertFalse("ct.isEmpty", ct.isEmpty)
    
    val Some((DifferentTypes(Left(ctl), Left(ctr)), tpct)) = ct
    assertTrue("ctl", ctl eq cd1)
    assertTrue("ctr", ctr eq t1)
    assertTrue("tpct is DummyContext", tpct eq DummyContext)
    
    val cd2 = CData("fred")
    assertTrue("cd1 === cd2", cd1 === cd2)
    val cd3 = CData("freedd")
    assertFalse("cd1 === cd3", cd1 === cd3)
    
    val c1 = Comment("fred")
    val c2 = Comment("fred")
    assertTrue("c1 === c2", c1 === c2)
    val c3 = Comment("freedd")
    assertFalse("c1 === c3", c1 === c3)
    
    val p1 = PI("fred","was")
    val p2 = PI("fred","was")
    assertTrue("p1 === p2", p1 === p2)
    val p3 = PI("freedd", "was")
    assertFalse("p1 === p3", p1 === p3)
    val p4 = PI("fred", "waaas")
    assertFalse("p1 === p4", p1 === p4)
    val p5 = PI("freedd", "waaas")
    assertFalse("p1 === p5", p1 === p5)
    
    
  }

  def testAttribute : Unit = {

    val a1 = Attribute("local"l, "value")
    val a2 = Attribute("local"l, "value")
    // can be true or false - luck
    assertTrue("a1 == a2", a1 == a2)
    assertTrue("a1 === a2", a1 === a2)

    val a3 = Attribute("ellocal"l, "value")
    assertFalse("a1 === a3", a1 === a3)

    val diffN = compare(DummyContext, a1, a3)
    assertFalse("diffN.isEmpty", diffN.isEmpty)

    val Some((AttributeNameDifference(diffNl, diffNr), DummyContext)) = diffN
    assertTrue("a1 eq diffNl", a1 eq diffNl)
    assertTrue("a3 eq diffNr", a3 eq diffNr)

    val a4 = Attribute("local"l, "another")
    assertFalse("a1 == a4", a1 == a4)
//    assertFalse("a1 === a4", Identity(a1).===(a4)(defaultAttributeEquals))

    val diffV = defaultAttributeComparison.compare(true, DummyContext, a1, a4)
    assertFalse("diffV.isEmpty", diffV.isEmpty)

    val Some((AttributeValueDifference(diffVl, diffVr), DummyContext)) = diffV
    assertTrue("a1 eq diffVl", a1 eq diffVl)
    assertTrue("a4 eq diffVr", a4 eq diffVr)

    val diffNc = defaultAttributeComparison.compare(false, DummyContext, a1, a3)
    assertFalse("diffNc.isEmpty", diffNc.isEmpty)

    val Some((SomeDifference(diffNcl, diffNcr), ComparisonContext(None, None, Nil, None))) = diffNc
    assertTrue("null eq diffNcl", null eq diffNcl)
    assertTrue("null eq diffNcr", null eq diffNcr)

    val diffVc = defaultAttributeComparison.compare(false, DummyContext, a1, a4)
    assertFalse("diffVc.isEmpty", diffVc.isEmpty)

    val Some((SomeDifference(diffVcl, diffVcr), ComparisonContext(None, None, Nil, None))) = diffVc
    assertTrue("null eq diffVl", a1 eq diffVl)
    assertTrue("null eq diffVr", a4 eq diffVr)    
  }

  def testAttributes : Unit = {
    import SomeDifference.noCalculation

    val a1 = Attribute(p("local"), "value")
    val a2 = Attribute(po("local"), "value")
    
    val a3 = Attribute(p("local1"), "value")
    val a4 = Attribute(po("local1"), "value")
    
    val a5 = Attribute(p("local2"), "value")
    val a6 = Attribute(po("local2"), "value")
    
    val attrs1 = Attribs(a1, a2, a3, a4, a5, a6)
    val attrs2 = Attribs(a1, a2, a3, a4, a5, a6)
    
    assertFalse("attrs1 == attrs2 - we expect this false as we don't define equality fully for ListSet, given equals", attrs1 == attrs2)
    assertTrue("attrs1 === attrs2", attrs1 === attrs2)
    
    val attrs3 = Attribs(a1, a3, a4, a5, a6)

    val wrongCount = compare(DummyContext, attrs1, attrs3)
    val Some((DifferentNumberOfAttributes(wcl, wcr), DummyContext)) = wrongCount
    assertTrue("wcl eq attrs1", wcl eq attrs1)
    assertTrue("wcr eq attrs3", wcr eq attrs3)

    val wrongCountnc = defaultAttributesComparison.compare(false, DummyContext, attrs1, attrs3)
    assertTrue("wrongCountnc eq noCalculation",wrongCountnc eq noCalculation)

    assertFalse("attrs3 === attrs1", attrs3 === attrs1)

    val missing = po("newlocal")
    val missinga = Attribute(missing, "value")
    
    val attrs4 = Attribs(a1, a3, a4, a5, a6, missinga)

    val leftMissing = compare(DummyContext, attrs4, attrs1)
    val Some((MissingAttributes(lml, lmr, lm), DummyContext)) = leftMissing

    assertTrue("lml eq attrs4", lml eq attrs4)
    assertTrue("lmr eq attrs1", lmr eq attrs1)
    assertTrue("lm eq missinga", lm eq missinga)

    val missingnc = defaultAttributesComparison.compare(false, DummyContext, attrs1, attrs4)
    assertTrue("missingnc eq noCalculation",missingnc eq noCalculation)

    val rightMissing = defaultAttributesComparison.compare(true, DummyContext, attrs1, attrs4)
    val Some((MissingAttributes(rml, rmr, rm), DummyContext)) = rightMissing

    assertTrue("rml eq attrs1", rml eq attrs1)
    assertTrue("rmr eq attrs4", rmr eq attrs4)
    assertTrue("rm eq a2", rm eq a2)
   
    val differentValuea = Attribute(missing, "another value")
    val attrs5 = Attribs(a1, a3, a4, a5, a6, differentValuea)

    val diffValue = defaultAttributesComparison.compare(true, DummyContext, attrs5, attrs4)
    val Some((DifferentValueAttributes(dvl, dvr, dva), DummyContext)) = diffValue

    assertTrue("dvl eq attrs5", dvl eq attrs5)
    assertTrue("dvr eq attrs4", dvr eq attrs4)
    assertTrue("dva eq differentValuea", dva eq differentValuea)
   
    assertFalse("attrs5 === attrs4", attrs5 === attrs4)


    val diffVnc = defaultAttributesComparison.compare(false, DummyContext, attrs5, attrs4)
    assertTrue("diffVnc eq noCalculation",diffVnc eq noCalculation)
    
  }

  def testXmlEqualsPrefixRelevant : Unit = {
    implicit val qnameEqual = equal { (a: QName, b: QName) => a ==== b }

    val attrs1 = Attribs("a1" -> "v1", "a2" -> "v2")
    val attrs2 = Attribs("a1" -> "v1", "a2" -> "v2")

    val pod = no.prefixed("po2")

    val elem1 = Elem(po("elem"), attrs1)
    val elem2 = Elem(pod("elem"), attrs2)

    // no prefixes
    assertTrue("attrs1 === attrs2", attrs1 === attrs2)
    // has prefixes
    assertFalse("elem1 === elem2", elem1 === elem2)       
  }

  def testQNameMix : Unit = {
    val nonp = no("elem") // namespaced but no prefix, differente types but same semantical meaning
    assertTrue("compare(qn, nonp).isEmpty", compare(qn, nonp).isEmpty)
  }

  def testXmlEqualsAttrsPrefixRelevant : Unit = {
    val prefixQNameEqual = equal { (a: QName, b: QName) => a ==== b }
    implicit val defaultAttributeComparison : XmlComparison[Attribute] = new AttributeComparison()(prefixQNameEqual, defaultQNameTokenComparison)

    val pod = no.prefixed("po2")

    val qn = po("a1")
    val qnd = pod("a1")

    assertTrue("qn === qnd", qn === qnd)
    assertFalse("qn ==== qnd", qn ==== qnd)
    assertTrue("compare(Nil, qn, qnd).isEmpty", compare(qn, qnd).isEmpty)

    val attr1 : Attribute = qn -> "v1"
    val attr2 : Attribute = qnd -> "v1"

    assertFalse(".equal toQName", prefixQNameEqual.equal(EqualsHelpers.toQName(attr1.name), EqualsHelpers.toQName(attr2.name)))
    
    assertFalse("attr1 === attr2", attr1 === attr2)
    assertTrue("compare(Nil, attr1, attr2).isDefined", compare(attr1, attr2).isDefined)

    val attrs1 = Attribs(attr1, "a2" -> "v2")
    val attrs2 = Attribs(attr2, "a2" -> "v2")

    val elem1 = Elem(po("elem"), attrs1)
    val elem2 = Elem(pod("elem"), attrs2)

    // no prefixes
    assertFalse("attrs1 === attrs2", attrs1 === attrs2)
    // has prefixes
    assertFalse("elem1 === elem2", elem1 === elem2)       
  }

  def testElems : Unit = {
    val a1 = Attribute(p("local"), "value")
    val a2 = Attribute(po("local"), "value")
    
    val a3 = Attribute(p("local1"), "value")
    val a4 = Attribute(po("local1"), "value")
    
    val a5 = Attribute(p("local2"), "value")
    val a6 = Attribute(po("local2"), "value")
    val a7 = Attribute(po("local3"), "value")
    
//    import ElemEquals._

    val attrs1 = Attribs(a1, a2, a3, a4, a5, a6)
    val attrs2 = Attribs(a1, a2, a3, a4, a5, a6)
    
    val attrs3 = Attribs(a1, a3, a4, a5, a6, a7)

    val elem1 = Elem(po("elem"), attrs1)
    val elem2 = Elem(po("elem"), attrs1)

    assertTrue("elem1 === elem2", elem1 === elem2)
    
    assertTrue("compare elem1 elem2 .isEmpty", compare(DummyContext, elem1, elem2).isEmpty)

    val elem3 = Elem(p("elem"), attrs1)
    assertFalse("elem1 === elem3", elem1 === elem3)
    
    val diffname = compare(DummyContext, elem1, elem3)
    val Some((ElemNameDifference(dnl, dnr), DummyContext)) = diffname
    
    assertTrue( "dnl eq elem1", dnl eq elem1)
    assertTrue( "dnr eq elem3", dnr eq elem3)
   
    val elem4 = Elem(po("elem"), attrs3)
    
    assertFalse("elem4 === elem1", elem4 === elem1)

    val diffattr = defaultElemComparison.compare(true, DummyContext, elem1, elem4)
    val Some((ElemAttributeDifference(dal, dar, MissingAttributes(mal, mar, ma)), DummyContext)) = diffattr

    assertTrue("dal eq elem1", dal eq elem1)
    assertTrue("dar eq elem4", dar eq elem4)

    assertTrue("mal eq elem1.attributes", mal eq elem1.attributes)
    assertTrue("mar eq elem1.attributes", mar eq elem4.attributes)
    assertTrue("ma eq a2", ma eq a2)
  }
  
  def testBasicPath : Unit = {


    val qn = po("elem")
    val qne = Elem(qn)

    // we should therefore get a 2
    val StartPath : BasicPath = one(("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 1)))

    val StartContext = ComparisonContext(StartPath)

    assertEquals("{}root", qualifiedName(StartPath))
    assertEquals("{}root", qualifiedName(StartContext))

    // the parent now has two as we are in this path.
    val EndPath : BasicPath = List((qn, Map()), ("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 2)))

    assertEquals("{uri:prefixed}elem", qualifiedName(EndPath))
    
    val testEmpty = ComparisonContext().startElems(qne, qne)
    val ((eeq, m : Map[String, Int]) :: Nil) = testEmpty.path
    assertTrue("eeq eq qn", eeq eq qn)
    assertTrue("m is empty", m.isEmpty)

    assertEquals("ps(testEmpty)", "/{uri:prefixed}elem[1]", testEmpty.pathString)
    
    val pop = testEmpty.endElem 
    assertTrue("pop's empty", pop.path.isEmpty)

    assertEquals("ps(pop)", "", pop.pathString)
    
    val testCounts = StartContext.startElems(qne, qne)
    testCounts.path match {
      case EndPath => true
      case _ => fail("start on Start did not End well")
    }

    assertEquals("ps(testCounts)","/{}root[1]/{uri:prefixed}elem[2]", testCounts.pathString)

    val Cont : BasicPath = List((qn, Map()), (qn, Map("{uri:prefixed}elem" -> 1)), ("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 2)))

    val testCont = ComparisonContext(EndPath).startElems(qne, qne)
    assertEquals("ps(testCont)", "/{}root[1]/{uri:prefixed}elem[2]/{uri:prefixed}elem[1]", testCont.pathString)

    val popped = testCont.endElem
    assertEquals("ps(popped)", "/{}root[1]/{uri:prefixed}elem[2]", popped.pathString)

    val up2 = popped.startElems(qne, qne) // we've gone from /1/2/1 to /1/2/2
    assertEquals("ps(up2)", "/{}root[1]/{uri:prefixed}elem[2]/{uri:prefixed}elem[2]", up2.pathString)

    val upped = up2.endElem.path.tail
    val pop2 = ComparisonContext(upped).startElems(qne, qne)
    assertEquals("ps(pop2)", "/{}root[1]/{uri:prefixed}elem[3]", pop2.pathString)
    
    val andDownAgain = pop2.startElems(qne, qne) // new parent elem, new count
    assertEquals("ps(andDownAgain)", "/{}root[1]/{uri:prefixed}elem[3]/{uri:prefixed}elem[1]", andDownAgain.pathString)
    
  }

  def testXPaths : Unit = {
    val nons = top(xml) \* 2
    val nons2 = top(xml2) \* 2
    
    assertTrue( "nons === nons2", nons.head === nons2.head)
  }

  def doStreamTest( streamEquals : StreamEquals ) = {
    import streamEquals._
    //xml xml2
    assertTrue("xml === xml", convertToStream(xml) === convertToStream(xml))
    assertTrue("xml === xml2", convertToStream(xml) === convertToStream(xml2))

    val nons = top(xml) \* 2
    val res = foldPositions(nons) {
      case p => Replace( p.tree.section.copy( attributes = Attribs("ah" -> "so")))
    }

    assertTrue("its left ", res.isLeft)

    val noAttribs = res.left.get
//    printTree(noAttribs.tree)
    assertFalse("xml === noAttribs", convertToStream(xml) === convertToStream(noAttribs.tree))
    
    // defaultStreamComparison. true
    val diff = compare(convertToStream(xml), convertToStream(noAttribs.tree))

    assertTrue("diff is some", diff.isDefined)

    val Some((ElemAttributeDifference(dal, dar, DifferentNumberOfAttributes(wcl, wcr)), context)) = diff
    
    assertEquals("/{urn:default}Default[1]/{}NoNamespace[1]", context.pathString)

    assertEquals("da1.name is NoNamespace", "{}NoNamespace", dal.name.qualifiedName)
    assertTrue("wcr has ah", wcr("ah"l).isDefined)
  }

  def testExactStreamEquals : Unit = {
    doStreamTest( ExactStreamEquals )
  }

  def testDefaultStreamEquals : Unit = {
    doStreamTest( ScalesXml )
  }

  def testDefaultXmlEquals : Unit = {
    val attrs1 = Attribs("a1" -> "v1", "a2" -> "v2")
    val attrs2 = Attribs("a1" -> "v1", "a2" -> "v2")

    val elem1 = Elem(po("elem"), attrs1)
    val elem2 = Elem(po("elem"), attrs2)

    assertTrue("attrs1 === attrs2", attrs1 === attrs2)
    assertTrue("elem1 === elem2", elem1 === elem2)   
  }

  def testJoinTextAndCData : Unit = {

    val root = po("root")
    val child = po("child")
    val sub = po("sub")

    import LogicalFilters._

    val x = <(root) /( "0","1",CData("2"),"3","4", 
      		child /( "s1", CData("s2"), "s3" ),
      		child /( CData("s22"), "s23" ),
		"5", CData("6") )

    assertEquals( "Should start with 9", 9 , x.toTree.children.size)

//printTree(new JoinTextAndCData(convertToStream(x)) : Iterator[PullType])

    val str = asString(joinTextAndCData(convertToStream(x)) : Iterator[PullType])

    assertEquals("Should have serialized to", """<?xml version="1.0" encoding="UTF-8"?><po:root xmlns:po="uri:prefixed">01234<po:child>s1s2s3</po:child><po:child>s22s23</po:child>56</po:root>""", str)

    val x2 = toTree(joinTextAndCData(convertToStream(x)) : Iterator[PullType])
    assertEquals( "Should now be 4", 4 , x2.toTree.children.size)

    val str2 = asString(joinText(convertToStream(x)) : Iterator[PullType])
    assertEquals("Should have serialized keeping CData to", """<?xml version="1.0" encoding="UTF-8"?><po:root xmlns:po="uri:prefixed">01<![CDATA[2]]>34<po:child>s1<![CDATA[s2]]>s3</po:child><po:child><![CDATA[s22]]>s23</po:child>5<![CDATA[6]]></po:root>""", str2)
   
  }

  def testRemovePIAndComments : Unit = {

    val root = po("root")
    val child = po("child")

    import LogicalFilters._

    val x = <(root) /( "0","1",CData("2"), Comment("c2"),"3","4", 
		      PI("i","s"),
      		child /( "s1", CData("s2"), Comment("cs2"), "s3" ),
      		child /( CData("s22"), Comment("cs22"), PI("i","s"), "s23" ),
		PI("i","s"), "5", CData("6"), Comment("c6") )
    
    val str = asString(removePIAndComments(joinText(convertToStream(x)) : Iterator[PullType]))
    assertEquals("Should have scrapped all comments", """<?xml version="1.0" encoding="UTF-8"?><po:root xmlns:po="uri:prefixed">01<![CDATA[2]]>34<po:child>s1<![CDATA[s2]]>s3</po:child><po:child><![CDATA[s22]]>s23</po:child>5<![CDATA[6]]></po:root>""", str)
    
  }

  def testRemovePIAndCommentsEqual : Unit = {

    val root = po("root")
    val child = po("child")

    import LogicalFilters._

    implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[T] = new StreamComparisonWrapper( new StreamComparison( x => removePIAndComments(joinText(x)))( ic, ec, qe, qnameTokenComparison) )

    val x1 = <(root) /( "0","1",CData("2"), Comment("c2"),"3","4", 
		      PI("i","s"),
      		  child /( "s1", CData("s2"), Comment("cs2"), "s3" ),
      		  child /( CData("s22"), Comment("cs22"), PI("i","s"), "s23" ),
		PI("i","s"), "5", CData("6"), Comment("c6") )

    val x2 = <(root) /( "0","1",CData("2"), "3","4", 
		  child /( "s1", CData("s2"), "s3" ),
      		  child /( CData("s22"), "s23" ),
		"5", CData("6") )
    
    assertTrue( " x1 === x2 ", x1 === x2)
  }

  /**
   * This test simply looks at the test package behaviour
   */
  def testDefault : Unit = {
    val root = po("root")
    val child = po("child")
    val sub = po("sub")

    val x1 = <(root) /( "0","1",CData("2"),"3","4", 
      		child /( "s1", CData("s2"), "s3" ),
      		child /( CData("s22"), "s23" ),
		"5", CData("6") )

    val x2 = <(root) /( "01",CData("23"),"4", 
      		child /( CData("s1"), CData("s2"), "s3" ),
      		child /( CData("s22s"), "23" ),
		"5", CData("6"), "" )

    assertTrue("x1 and x2 should be equal", x1 === x2)

    // comments and pi are kept
    val x3 = <(root) /( "01",CData("23"),"4", 
      		child /( CData("s1"), Comment("comment"), CData("s2"), "s3" ),
      		child /( CData("s22s"), "23" ),
		"5", CData("6"), "" )

    assertFalse("x2 and x3 should not be equal", convertToStream(x2.toTree) === convertToStream(x3.toTree))
  }

  val npo = no.prefixed("npo")
  val x1 = po("root") /( po("child") ~> "po:value" )
  val x2 = npo("root") /( npo("child") ~> "npo:value" )

  val x1_2 = po("root") /( po("child") ~> "po:value" )

  def testNormalQNames : Unit = {
    // the two documents are logically the same, but straight comparison will be different:
    assertFalse("x1 === x2 - before qname handling", x1 === x2)
  }

  // can be in a text or attribute node
  def testEmbeddedQNames : Unit = {
    import SomeDifference._

    implicit val defaultQNameTokenComparison = Option(qnamesEqual _)

    val noNS = po("root") /( po("child") ~> "pof:value" )
    
    assertTrue("x1 === x2 - qname is in", x1 === x2)
    assertTrue("x1 === x1_2 ", x1 === x1_2)

    // the prefix isn't there
    assertFalse("x1 === noNS", x1 === noNS)
    
    // swap
    assertFalse("noNS === x1", noNS === x1)
    
    // defined but different
    val el = Elem(po("child")).copy( namespaces = Map( "pof" -> "uri:has" ))
    val hasNS = po("root") /( el ~> "pof:value" )
    assertFalse("x1 === hasNS", x1 === hasNS)
    
    // swap
    assertFalse("hasNS === x1", hasNS === x1)
    
    // sanity
    assertFalse("hasNS === noNS", hasNS === noNS)
    assertFalse("noNS === hasNS", noNS === hasNS)
    
    // mixed up, taken from root
    val m1 = npo("root") /( po("child") ~> "npo:value" )
    val m2 = po("root") /( npo("child") ~> "po:value" )
    assertTrue("m1 === m2 ", m1 === m2)

    val m2_np = po("root") /( npo("child") ~> "value" )
    assertFalse("m1 === m2_np ", m1 === m2_np)
  }

  val ax1 = po("root") /( po("child") /@("loc" -> "po:value", "a" -> "a") )
  val ax2 = npo("root") /( npo("child") /@("loc" -> "npo:value", "a" -> "a") )

  def testNormalAttrQNames : Unit = {
    assertFalse("ax1 === ax2", ax1 === ax2)
  }

  def testEmbeddedAttrQNames : Unit = {
    implicit val defaultQNameTokenComparison : Option[(ComparisonContext, String, String) => Boolean] = Some{(c, s ,s2) => qnamesEqual(c,s,s2)}

    assertTrue("ax1 === ax2", ax1 === ax2)
  }

  val miscDiffL1 = Left(Comment("another"))
  val miscDiffL2 = Left(Comment("another"))
  val miscDiffR = Right(PI("what","where"))
  val miscDiffContentR = Left(Comment("yet another"))

  val misc1 = List[Either[Comment, PI]](Left(Comment("A comment")), miscDiffL1, Right(PI("what","where")))
  val misc2 = List[Either[Comment, PI]](Left(Comment("A comment")), miscDiffL2, Right(PI("what","where")))
  val miscN = List[Either[Comment, PI]](Left(Comment("A comment")), Right(PI("what","where")))
  val miscD = List[Either[Comment, PI]](Left(Comment("A comment")), miscDiffR, Left(Comment("another")))
  val miscDContent = List[Either[Comment, PI]](Left(Comment("A comment")), miscDiffContentR, Right(PI("what","where")))

  val d1 = Doc(x1)
  val d2 = Doc(x2)
  val d1_2 = Doc(x1_2)

  // same doc and same miscs
  val m1 = d1.copy(prolog = d1.prolog.copy(misc = misc1), end = EndMisc(misc2))
  val m2 = d1_2.copy(prolog = d1_2.prolog.copy(misc = misc2), end = EndMisc(misc1))

  def testDocHandling : Unit = {
    assertFalse("d1 shouldn't === d2", d1 === d2)

    assertTrue("d1 should === d1_2", d1 === d1_2)

    assertTrue("m1 should === m2", m1 === m2)
  }

  def testDifferentNumberOfMiscs : Unit = {
    // swap the first out
    val c1 = m2.copy(prolog = m2.prolog.copy(misc = miscN))

    assertFalse("m2 should not === c1", m2 === c1)

    val countRes = compare(m2, c1)
    assertTrue("countRes should be defined", countRes.isDefined)

    val Some((DifferentNumberOfMiscs(cl,cr,true), ccontext)) = countRes
    assertTrue("cl eq misc2", cl eq misc2)
    assertTrue("cr eq miscN", cr eq miscN)

    // swap the end out
    val c2 = m2.copy(end = EndMisc(miscN))
    
    val ncountRes = compare(m2, c2)
    assertTrue("ncountRes should be defined", ncountRes.isDefined)

    val Some((DifferentNumberOfMiscs(ncl,ncr,false), nccontext)) = ncountRes
    assertTrue("ncl eq misc1", ncl eq misc1) // ends are different on purpose
    assertTrue("ncr eq miscN", ncr eq miscN)
  }


  def testMiscDifferentTypes : Unit = {
    // swap the first out
    val c1 = m2.copy(prolog = m2.prolog.copy(misc = miscD))

    assertFalse("m2 should not === c1", m2 === c1)

    val miscRes = compare(m2, c1)
    assertTrue("miscRes should be defined", miscRes.isDefined)

    val Some((MiscDifferentTypes(ml,mr,true), mcontext)) = miscRes
    assertTrue("ml eq miscDiffL1", ml eq miscDiffL2)
    assertTrue("mr eq miscDiffR", mr eq miscDiffR)

    // swap the end out
    val c2 = m2.copy(end = EndMisc(miscD))
    
    val nmiscRes = compare(m2, c2)
    assertTrue("ncountRes should be defined", nmiscRes.isDefined)

    val Some((MiscDifferentTypes(nml, nmr,false), nmcontext)) = nmiscRes
    assertTrue("nml eq miscDiffL1", nml eq miscDiffL1) // ends are different on purpose
    assertTrue("nmr eq miscDiffR", nmr eq miscDiffR)
  }

  def testMiscDifference : Unit = {
    // swap the first out
    val c1 = m2.copy(prolog = m2.prolog.copy(misc = miscDContent))

    assertFalse("m2 should not === c1", m2 === c1)

    val miscRes = compare(m2, c1)
    assertTrue("miscRes should be defined", miscRes.isDefined)

    val Some((MiscDifference(ml,mr,true), mcontext)) = miscRes
    assertTrue("ml eq miscDiffL1", ml eq miscDiffL2)
    assertTrue("mr eq miscDiffR", mr eq miscDiffContentR)

    // swap the end out
    val c2 = m2.copy(end = EndMisc(miscDContent))
    
    val nmiscRes = compare(m2, c2)
    assertTrue("ncountRes should be defined", nmiscRes.isDefined)

    val Some((MiscDifference(nml, nmr,false), nmcontext)) = nmiscRes
    assertTrue("nml eq miscDiffL1", nml eq miscDiffL1) // ends are different on purpose
    assertTrue("nmr eq miscDiffR", nmr eq miscDiffContentR)
  }
}
