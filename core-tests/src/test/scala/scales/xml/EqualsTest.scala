package scales.xml

class EqualsTest extends junit.framework.TestCase {
  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._
  import scales.xml.equals._

  import Functions._

  val xmlFile = loadXml(resource(this, "/data/BaseXmlTest.xml"))
  val xml = xmlFile.rootElem
  val xmlFile2 = loadXml(resource(this, "/data/BaseXmlTest.xml"))
  val xml2 = xmlFile2.rootElem

  val ns = Namespace("urn:default")

  // attributes are here
  val nons = ( top(xml) \* 2 ).head
  val nons2 = ( top(xml2) \* 2 ).head

  // these guys import a resource
  import scalaz._
  import Scalaz._

  def testItems : Unit = {

    import scales.xml.equals.ItemEquals._
    
    val t1 = Text("fred")
    val t2 = Text("fred")

    assertTrue("t1 == t2", t1 == t2)
    assertTrue("t1 === t2", t1 === t2)

    assertTrue("t1 compare t2", DefaultXmlItemComparisom.compare(true, Nil, t1, t2).isEmpty )
    
    val t3 = Text("freed")

    assertFalse("t1 == t3", t1 == t3)
    assertFalse("t1 === t3", t1 === t3)

    val t1and3c = DefaultXmlItemComparisom.compare(true, Nil, t1, t3)
    assertFalse("t1and3c.isEmpty", t1and3c.isEmpty )
    
    val Some((ItemDifference(t13cl, t13cr), tp)) = t1and3c 
    assertTrue("t13cl", t13cl eq t1)
    assertTrue("t13cr", t13cr eq t3)
    
    val t1and3nc = DefaultXmlItemComparisom.compare(false, Nil, t1, t3)
    assertFalse("t1and3nc.isEmpty", t1and3nc.isEmpty )
    
    val Some((SomeDifference(t13ncl, t13ncr), tpn)) = t1and3nc 
    assertTrue("t13ncl", t13ncl eq null)
    assertTrue("t13ncr", t13ncr eq null)
    
    val cd1 = CData("fred")
    
    val ct = DefaultXmlItemComparisom.compare(true, Nil, cd1, t1)
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
    import scales.xml.equals.AttributeEquals._

    val a1 = Attribute("local"l, "value")
    val a2 = Attribute("local"l, "value")
    assertTrue("a1 == a2", a1 == a2)
    assertTrue("a1 === a2", a1 === a2)

    val a3 = Attribute("ellocal"l, "value")
    assertFalse("a1 === a3", a1 === a3)

    val diffN = defaultAttributeComparisom.compare(true, Nil, a1, a3)
    assertFalse("diffN.isEmpty", diffN.isEmpty)

    val Some((AttributeNameDifference(diffNl, diffNr), diffNp)) = diffN
    assertTrue("a1 eq diffNl", a1 eq diffNl)
    assertTrue("a3 eq diffNr", a3 eq diffNr)

    val a4 = Attribute("local"l, "another")
    assertFalse("a1 == a4", a1 == a4)
    assertFalse("a1 === a4", Identity(a1).===(a4)(defaultAttributeEquals))

    val diffV = defaultAttributeComparisom.compare(true, Nil, a1, a4)
    assertFalse("diffV.isEmpty", diffV.isEmpty)

    val Some((AttributeValueDifference(diffVl, diffVr), diffVp)) = diffV
    assertTrue("a1 eq diffVl", a1 eq diffVl)
    assertTrue("a4 eq diffVr", a4 eq diffVr)

    val diffNc = defaultAttributeComparisom.compare(false, Nil, a1, a3)
    assertFalse("diffNc.isEmpty", diffNc.isEmpty)

    val Some((SomeDifference(diffNcl, diffNcr), Nil)) = diffNc
    assertTrue("null eq diffNcl", null eq diffNcl)
    assertTrue("null eq diffNcr", null eq diffNcr)

    val diffVc = defaultAttributeComparisom.compare(false, Nil, a1, a4)
    assertFalse("diffVc.isEmpty", diffVc.isEmpty)

    val Some((SomeDifference(diffVcl, diffVcr), Nil)) = diffVc
    assertTrue("null eq diffVl", a1 eq diffVl)
    assertTrue("null eq diffVr", a4 eq diffVr)    
  }

  /**
   * Provided but I really can't recommend anyone to use it
   */ 
  def testAttributePrefix : Unit = {
    val n = Namespace("uri:prefix")
    val p = n.prefixed("p")
    val po = n.prefixed("po")

    val a1 = Attribute(p("local"), "value")
    val a2 = Attribute(po("local"), "value")

    // sanity check
    {
      import scales.xml.equals.AttributeEquals._

      assertTrue("a1 == a2", a1 == a2)
      assertTrue("a1 === a2 with normal prefix ignored", a1 === a2)
    }

    import scales.xml.equals.AttributeEquals.ExactQName._

    assertTrue("a1 == a2", a1 == a2)
    assertFalse("a1 === a2", a1 === a2)

    val diffN = prefixAttributeComparisom.compare(true, Nil, a1, a2)
    assertFalse("diffN.isEmpty", diffN.isEmpty)

    val Some((AttributeNameDifference(diffNl, diffNr), diffNp)) = diffN
    assertTrue("a1 eq diffNl", a1 eq diffNl)
    assertTrue("a3 eq diffNr", a2 eq diffNr)
  }
}
