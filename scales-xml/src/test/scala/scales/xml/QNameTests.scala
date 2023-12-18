package scales.xml

class QNameTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._
  import java.io._
  
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  val ns = Namespace("test:uri")
  val nsa = Namespace("test:uri:attribs")
  val nsp = nsa.prefixed("pre")

  def testSameObjectsEquals = {
    val ns1 = Namespace("test:uri")
    assertEquals(ns1, ns)

    val nsp2 = nsa.prefixed("pre")
    assertEquals(nsp, nsp2)

    val name1 = nsp("plocal")
    val name2 = nsp("plocal")

    assertEquals(name1, name2)

    val nons = NoNamespaceQName("nnlocal")
    val nons2 = NoNamespaceQName("nnlocal")

    assertEquals( nons, nons2 )

    val nopname = ns1("noplocal")
    val nopname1 = ns1("noplocal")

    assertEquals( nopname, nopname1)
  }

  def testSameNamespaceEquals = {
    val name = ns.prefixed("pre", "local")
    val name1 = ns("local")

    assertEquals(name, name1)

    assertEquals(name, "test:uri"::"local" : QName)

    assertTrue("=:= should match on ns and local only", name =:= name1)
    assertFalse("=== should not have matched", name ==== name1)
  }

  def testExactMatch =  {
    val name = ns.prefixed("pre", "local")
    val name1 = ns("local")

    assertFalse("Should not be exactly equal, one is prefixed", name ==== name1)

    assertFalse("Should not be exactly equal, one is prefixed", name ==== ("test:uri"::"local" : QName))

    val name3 = ns.prefixed("pre","local")

    assertTrue("Should match exactly", name ==== name3)

    assertTrue("=:= should also of course  match", name =:= name3)
  }

  def testLocalOnlyMatch = {
    val name = "LocalOnly"l;
    val name2 = "LocalOnly"l;
    assertEquals(scales.xml.impl.NamespaceDefaults.noNamespace, name.namespace)
    assertTrue("Should have been =:=",name =:= name2)
    assertEquals(name, name2)
  }

  def testAttributeMap = {
    val name1 = ns.prefixed("pre", "local")
    val name2 = ns{"local"}
    val non = "none"l;
    val map = Map[QName, String]( name1 -> "fred", non -> "james")
    assertEquals("fred", map(name2))
  }

  def testMismatchedPre = {
    val xml = Namespace(Namespace.xmlNS)
    val xmlns = Namespace(Namespace.xmlnsNS)

    def validateTest( ns : Namespace, nsPre : String ) =
      try {
	val x = PrefixedNamespace(ns, "fred")
	fail("Did not throw when validating "+nsPre+" prefix")
      } catch {
	case t : Throwable => 
	  assertTrue("Did not get expected error for "+nsPre+": "+t.getMessage, t.getMessage.indexOf("bound to prefix '"+nsPre+"'") > -1)
      }

    validateTest(xml, "xml")
    validateTest(xmlns, "xmlns")
    
    // sanity checks
    val xmlpre = PrefixedNamespace(xml, PrefixedNamespace.xmlPRE)
    val xmlnspre = PrefixedNamespace(xmlns, PrefixedNamespace.xmlnsPRE)

    assertEquals("xmlPRE", PrefixedNamespace.xmlPRE, xmlpre.prefix)
    assertEquals("xmlNS", Namespace.xmlNS, xmlpre.ns.uri)
    assertEquals("xmlns.prefix", PrefixedNamespace.xmlnsPRE, xmlnspre.prefix)
    assertEquals("xmlns ns and uri", Namespace.xmlnsNS, xmlnspre.ns.uri)

  }

  /**
   * Funny test this, a number of the examples and tests I'd worked on had 0-9 in them, which is illegal in the spec :>.  Shows the value in adding the validation and also in paying attention to the problem (folding) than the legalities...
   */ 
  def testInvalidStartChar = {
    try {
      val q = Namespace("urn:fred").prefixed("pre","3")
      fail("accepted 3 for some strange reason")
    } catch {
      case t : Throwable => assertTrue("Did not match invalidLocalName "+t.getMessage, t.getMessage.indexOf("The local name '3' is not valid") > -1)
    }
  }
  
  /**
   * Test the xml and xmlns prefixes for an element.
   */ 
  def testInvalidElemNames = {
    val xml = Namespace(Namespace.xmlNS).prefixed(PrefixedNamespace.xmlPRE)
    val xmlns = Namespace(Namespace.xmlnsNS).prefixed(PrefixedNamespace.xmlnsPRE)
 
    def validateTest( ns : PrefixedNamespace, nsPre : String ) =
      try {
	val x = Elem(ns("fred"))
	fail("Did not throw when validating "+nsPre+" prefix")
      } catch {
	case t : Throwable => assertTrue("Did not get expected error for "+nsPre+": "+t.getMessage, t.getMessage.indexOf("not allowed for elements") > -1)
      }

    validateTest(xml, "xml")
    validateTest(xmlns, "xmlns")

    // sanity
    val x = Elem(nsp("fred"))

  }
  
  /**
   * Tests both the compilation of alternatives and that they work
   */ 
  def testQNameImplicits = {
    val ns = Namespace("uri:namespace")
    val pre = ns.prefixed("pre")
    
    val unprefixedQName = ns("localName")
    val prefixedQName = pre("localName")
    val nonamespaceQName = "localName"l
    val nonamespaceQName2 = "localName2"l

    val uelem = Elem(unprefixedQName)
    
    val nonamespaceAQN : AttributeQName = 
      nonamespaceQName

    val root = 
      <(uelem) /@(Attribute(nonamespaceQName2,"mvp"),
		  nonamespaceAQN -> "nv",
		  prefixedQName -> "pv") /(
	prefixedQName, 
	nonamespaceQName 
      )
    
    val s = asString(root)
    assertTrue("Should have the mvp", s.indexOf("localName2=\"mvp\"") > -1)
    assertTrue("Should have the nv", s.indexOf("localName=\"nv\"") > -1)
    assertTrue("Should have the pv", s.indexOf("pre:localName=\"pv\"") > -1)

  }

  def testXSI = {
    val xtrue = ("alocal"l) /@(xsiNil -> "true")
    val xfalse = ("alocal"l) /@(xsiNil -> "false")
    val x1 = ("alocal"l) /@(xsiNil -> "1")
    val x0 = ("alocal"l) /@(xsiNil -> "0")

    val xrand = ("alocal"l) /@(xsiNil -> "rand")
    val control: XmlTree = ("alocal"l)

    assertTrue("true", isNil(xtrue))
    assertTrue("x1", isNil(x1))
    assertFalse("false", isNil(xfalse))
    assertFalse("x0", isNil(x0))
    assertFalse("rand", isNil(xrand))
    assertFalse("control", isNil(control))

  }
}
