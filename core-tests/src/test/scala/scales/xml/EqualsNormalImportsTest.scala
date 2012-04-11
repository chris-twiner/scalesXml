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

  // make sure its passed through where it should be
  val DummyPath : BasicPaths.BasicPath = List(("n"l, Map()))

  def testItems : Unit = {

    val t1 = Text("fred")
    val t2 = Text("fred")

    assertTrue("t1 == t2", t1 == t2)
    assertTrue("t1 === t2", t1 === t2)

    assertTrue("t1 compare t2", compare(Nil, t1, t2).isEmpty )
    
    val t3 = Text("freed")

    assertFalse("t1 == t3", t1 == t3)
    assertFalse("t1 === t3", t1 === t3)

    val t1and3c = defaultXmlItemComparison.compare(true, DummyPath, t1, t3)
    assertFalse("t1and3c.isEmpty", t1and3c.isEmpty )
    
    val Some((ItemDifference(t13cl, t13cr), DummyPath)) = t1and3c 
    assertTrue("t13cl", t13cl eq t1)
    assertTrue("t13cr", t13cr eq t3)
    
    val t1and3nc = defaultXmlItemComparison.compare(false, Nil, t1, t3)
    assertFalse("t1and3nc.isEmpty", t1and3nc.isEmpty )
    
    val Some((SomeDifference(t13ncl, t13ncr), tpn)) = t1and3nc 
    assertTrue("t13ncl", t13ncl eq null)
    assertTrue("t13ncr", t13ncr eq null)
    
    val cd1 = CData("fred")
    
    val ct = defaultXmlItemComparison.compare(true, Nil, cd1, t1)
    assertFalse("ct.isEmpty", ct.isEmpty)
    
    val Some((DifferentTypes(Left(ctl), Left(ctr)), tpct)) = ct
    assertTrue("ctl", ctl eq cd1)
    assertTrue("ctr", ctr eq t1)
    
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

    val diffN = compare(DummyPath, a1, a3)
    assertFalse("diffN.isEmpty", diffN.isEmpty)

    val Some((AttributeNameDifference(diffNl, diffNr), DummyPath)) = diffN
    assertTrue("a1 eq diffNl", a1 eq diffNl)
    assertTrue("a3 eq diffNr", a3 eq diffNr)

    val a4 = Attribute("local"l, "another")
    assertFalse("a1 == a4", a1 == a4)
//    assertFalse("a1 === a4", Identity(a1).===(a4)(defaultAttributeEquals))

    val diffV = defaultAttributeComparison.compare(true, DummyPath, a1, a4)
    assertFalse("diffV.isEmpty", diffV.isEmpty)

    val Some((AttributeValueDifference(diffVl, diffVr), DummyPath)) = diffV
    assertTrue("a1 eq diffVl", a1 eq diffVl)
    assertTrue("a4 eq diffVr", a4 eq diffVr)

    val diffNc = defaultAttributeComparison.compare(false, DummyPath, a1, a3)
    assertFalse("diffNc.isEmpty", diffNc.isEmpty)

    val Some((SomeDifference(diffNcl, diffNcr), Nil)) = diffNc
    assertTrue("null eq diffNcl", null eq diffNcl)
    assertTrue("null eq diffNcr", null eq diffNcr)

    val diffVc = defaultAttributeComparison.compare(false, DummyPath, a1, a4)
    assertFalse("diffVc.isEmpty", diffVc.isEmpty)

    val Some((SomeDifference(diffVcl, diffVcr), Nil)) = diffVc
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

    val wrongCount = compare(DummyPath, attrs1, attrs3)
    val Some((DifferentNumberOfAttributes(wcl, wcr), DummyPath)) = wrongCount
    assertTrue("wcl eq attrs1", wcl eq attrs1)
    assertTrue("wcr eq attrs3", wcr eq attrs3)

    val wrongCountnc = defaultAttributesComparison.compare(false, DummyPath, attrs1, attrs3)
    assertTrue("wrongCountnc eq noCalculation",wrongCountnc eq noCalculation)

    assertFalse("attrs3 === attrs1", attrs3 === attrs1)

    val missing = po("newlocal")
    val missinga = Attribute(missing, "value")
    
    val attrs4 = Attribs(a1, a3, a4, a5, a6, missinga)

    val leftMissing = compare(DummyPath, attrs4, attrs1)
    val Some((MissingAttributes(lml, lmr, lm), DummyPath)) = leftMissing

    assertTrue("lml eq attrs4", lml eq attrs4)
    assertTrue("lmr eq attrs1", lmr eq attrs1)
    assertTrue("lm eq missinga", lm eq missinga)

    val missingnc = defaultAttributesComparison.compare(false, DummyPath, attrs1, attrs4)
    assertTrue("missingnc eq noCalculation",missingnc eq noCalculation)

    val rightMissing = defaultAttributesComparison.compare(true, DummyPath, attrs1, attrs4)
    val Some((MissingAttributes(rml, rmr, rm), DummyPath)) = rightMissing

    assertTrue("rml eq attrs1", rml eq attrs1)
    assertTrue("rmr eq attrs4", rmr eq attrs4)
    assertTrue("rm eq a2", rm eq a2)
   
    val differentValuea = Attribute(missing, "another value")
    val attrs5 = Attribs(a1, a3, a4, a5, a6, differentValuea)

    val diffValue = defaultAttributesComparison.compare(true, DummyPath, attrs5, attrs4)
    val Some((DifferentValueAttributes(dvl, dvr, dva), DummyPath)) = diffValue

    assertTrue("dvl eq attrs5", dvl eq attrs5)
    assertTrue("dvr eq attrs4", dvr eq attrs4)
    assertTrue("dva eq differentValuea", dva eq differentValuea)
   
    assertFalse("attrs5 === attrs4", attrs5 === attrs4)


    val diffVnc = defaultAttributesComparison.compare(false, DummyPath, attrs5, attrs4)
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

  def testXmlEqualsAttrsPrefixRelevant : Unit = {
    val prefixQNameEqual = equal { (a: QName, b: QName) => a ==== b }
    implicit val defaultAttributeComparison : XmlComparison[Attribute] = new AttributeComparison()(prefixQNameEqual)

    val pod = no.prefixed("po2")

    val qn = po("a1")
    val qnd = pod("a1")

    assertTrue("qn === qnd", qn === qnd)
    assertFalse("qn ==== qnd", qn ==== qnd)
    assertTrue("compare(Nil, qn, qnd).isEmpty", compare[QName](Nil, qn, qnd).isEmpty)

    val attr1 : Attribute = qn -> "v1"
    val attr2 : Attribute = qnd -> "v1"

    assertFalse(".equal toQName", prefixQNameEqual.equal(EqualsHelpers.toQName(attr1.name), EqualsHelpers.toQName(attr2.name)))
    
    assertFalse("attr1 === attr2", attr1 === attr2)
    assertTrue("compare(Nil, attr1, attr2).isDefined", compare(Nil, attr1, attr2).isDefined)

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
    
    assertTrue("compare elem1 elem2 .isEmpty", compare(DummyPath, elem1, elem2).isEmpty)

    val elem3 = Elem(p("elem"), attrs1)
    assertFalse("elem1 === elem3", elem1 === elem3)
    
    val diffname = compare(DummyPath, elem1, elem3)
    val Some((ElemNameDifference(dnl, dnr), DummyPath)) = diffname
    
    assertTrue( "dnl eq elem1", dnl eq elem1)
    assertTrue( "dnr eq elem3", dnr eq elem3)
   
    val elem4 = Elem(po("elem"), attrs3)
    
    assertFalse("elem4 === elem1", elem4 === elem1)

    val diffattr = defaultElemComparison.compare(true, DummyPath, elem1, elem4)
    val Some((ElemAttributeDifference(dal, dar, MissingAttributes(mal, mar, ma)), DummyPath)) = diffattr

    assertTrue("dal eq elem1", dal eq elem1)
    assertTrue("dar eq elem4", dar eq elem4)

    assertTrue("mal eq elem1.attributes", mal eq elem1.attributes)
    assertTrue("mar eq elem1.attributes", mar eq elem4.attributes)
    assertTrue("ma eq a2", ma eq a2)
  }
  
  def testBasicPath : Unit = {

    val qn = po("elem")

    // we should therefore get a 2
    val StartPath : BasicPath = one(("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 1)))
    // the parent now has two as we are in this path.
    val EndPath : BasicPath = List((qn, Map()), ("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 2)))

    
    val testEmpty = startElem(qn, Nil)
    val ((eeq, m : Map[String, Int]) :: Nil) = testEmpty
    assertTrue("eeq eq qn", eeq eq qn)
    assertTrue("m is empty", m.isEmpty)

    assertEquals("ps(testEmpty)", "/{uri:prefixed}elem[1]", pathString(testEmpty))
    
    val pop = endElem( testEmpty ) 
    assertTrue("pop's empty", pop.isEmpty)

    assertEquals("ps(pop)", "", pathString(pop))
    
    val testCounts = startElem(qn, StartPath)
    testCounts match {
      case EndPath => true
      case _ => fail("start on Start did not End well")
    }

    assertEquals("ps(testCounts)","/{}root[1]/{uri:prefixed}elem[2]", pathString(testCounts))

    val Cont : BasicPath = List((qn, Map()), (qn, Map("{uri:prefixed}elem" -> 1)), ("root"l, Map("{uri:prefixed}parent" -> 1, "{uri:prefixed}elem" -> 2)))

    val testCont = startElem(qn, EndPath)
    assertEquals("ps(testCont)", "/{}root[1]/{uri:prefixed}elem[2]/{uri:prefixed}elem[1]", pathString(testCont))

    val popped = endElem(testCont)
    assertEquals("ps(popped)", "/{}root[1]/{uri:prefixed}elem[2]", pathString(popped))

    val up2 = startElem(qn, popped) // we've gone from /1/2/1 to /1/2/2
    assertEquals("ps(up2)", "/{}root[1]/{uri:prefixed}elem[2]/{uri:prefixed}elem[2]", pathString(up2))

    val pop2 = startElem(qn, endElem(endElem(up2)))
    assertEquals("ps(pop2)", "/{}root[1]/{uri:prefixed}elem[3]", pathString(pop2))
    
    val andDownAgain = startElem(qn, pop2) // new parent elem, new count
    assertEquals("ps(andDownAgain)", "/{}root[1]/{uri:prefixed}elem[3]/{uri:prefixed}elem[1]", pathString(andDownAgain))
    
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
    val diff = compare(Nil, convertToStream(xml), convertToStream(noAttribs.tree))

    assertTrue("diff is some", diff.isDefined)

    val Some((ElemAttributeDifference(dal, dar, DifferentNumberOfAttributes(wcl, wcr)), path)) = diff
    
    assertEquals("/{urn:default}Default[1]/{}NoNamespace[1]", pathString(path))

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

    implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName]) : XmlComparison[T] = new StreamComparisonWrapper( new StreamComparison( x => removePIAndComments(joinText(x)))( ic, ec, qe) )

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


}
