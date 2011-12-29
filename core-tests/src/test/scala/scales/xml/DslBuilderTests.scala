package scales.xml

class DslBuildersTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import ScalesUtils._

  import ScalesXml._

  val ns = Namespace("test:uri")
  val nsa = Namespace("test:uri:attribs")
  val nsp = nsa.prefixed("pre")

  /*
 * NOTE the print does not work, as the following example nodes have different namespaces, each attrib should be checked against the namespace
 * of the elem.
 */

  def testAttributes = {
    val builder = <(ns { "Elem" })
    val withAttribs = builder /@ (nsa("pre", "attr1") -> "val1", "attr2" -> "val2", nsp { "attr3" } -> "val3")
    //printTree(withAttribs)
    assertTrue("doesn't have attrib", withAttribs.section.attributes.contains(nsa("pre", "attr1") ))
    val withoutAttribs = builder -/@ (nsa { "attr1" }, NoNamespaceQName { "attr2" }, nsa { "attr3" })
    /*println()
    printTree(withoutAttribs)
    println()*/
    assertFalse("has attrib", withoutAttribs.section.attributes.contains(nsa("pre", "attr1") ))
  }

  def testElems = {
    val builder = <(ns("Elem")) / (ns("Child"), ns("Child2"), ns("Child")) /( <(ns("fred")) ) /(Text("some text"))
    //printTree(builder)

    assertTrue("Doesn't have the child elems", (top(builder).\*(ns("Child")).size == 2))
    //println()

    val withoutChild = builder -/ (ns("fred"))
    //printTree(withoutChild)
    //println()

    assertTrue("Should still have the Child elems", (top(withoutChild).\*(ns("Child")).size == 2))
    assertTrue("Should still have had the Child2 elem", (top(withoutChild).\*(ns("Child2")).size == 1))
    assertTrue("Should not have had the fred elem", (top(withoutChild).\*(ns("fred")).size == 0))
  }

  def testQNames = {
    val builder = ns("Root") /( "Child"l ) // the l for localonly is needed to help the type checking
    assertEquals("", builder.children(0).right.get.section.name.namespace.uri)
  }

  def testTextNodes = {
    val builder = ns("Elem") / (<(ns("Child"))) / (Text("some text"), Text("Some more text"), Text("yet more"))

    assertTrue("Should have had 4 children", builder.children.size == 4)

    val cleaned = builder.elementsOnly

    assertTrue("Should have had only 1 child", cleaned.children.size == 1)
    assertTrue("Only child should be an elem", cleaned.children(0).isRight)

    val reset = builder ~> "Just the facts mam"

    assertTrue("Should have had only 1 child", reset.children.size == 1)
    assertTrue("Only child should be a text node", reset.children(0).isLeft)
    assertEquals("Just the facts mam", reset.children(0).left.get.value)
  }

  def testQNameMatchers : Unit = {
    val ens = ns("Elem")
    val ENS = ens.m
    val NEGENS = ns{"Elem2"}.m
    val ans = ns.prefixed("pre", "attr")
    val ANS = ans.m
    val local = "local"l
    val LOCAL = local.matcher

    val elem = Elem(ens)
    val attrib = Attribute(ans,"val")
    val attribnn = Attribute(local,"val")
    

    elem match {
      case ENS(e) => 1//ok
      case _ => assertFalse("Did not match elem from qname",true)
    }

    attrib match {
      case ANS(a) => 1//ok
      case _ => assertFalse("Did not match attr from qname",true)
    }

    attribnn match {
      case LOCAL(la) => 1//ok
      case _ => assertFalse("Did not match attr from qname no namespace",true)
    }

    elem match {
      case NEGENS(e) => fail("Should not have matched Elem2")
      case _ => 1//ok
    }

    attrib match {
      case LOCAL(a) => fail("Should not have matched local")
      case _ => 1 // ok
    }

    attribnn match {
      case ANS(la) => fail("Should not have matched prefixed ns")
      case _ => 1//ok
    }
    
  }

  def testPathMatcher : Unit = {
    val builder = <(ns { "Elem" })
    val withAttribs = builder /@ (nsa("pre", "attr1") -> "val1", "attr2" -> "val2", nsp { "attr3" } -> "val3")
    
    val PathMatcher = pathMatcher{ (t) => top(t).*@ }
    val PathAndTreeMatcher = pathAndTreeMatcher{ (t) => top(t).*@ }

    withAttribs.toTree match {
      case PathMatcher(paths) => 
	assertEquals(3, paths.size)
	
      case _ => assertFalse("PathMatcher did not match",true)
    }

    withAttribs.toTree match {
      case PathAndTreeMatcher(paths, tree) => 
	assertEquals(3, paths.size)
	assertEquals(3, tree.section.attributes.size)
	
      case _ => assertFalse("PathAndTreeMatcher did not match",true)
    }

    // negs
    builder.toTree match {
      case PathMatcher(paths) => 
	fail("PathMatcher should not have matched against builder")
	
      case _ => ()
    }

    builder.toTree match {
      case PathAndTreeMatcher(paths, tree) => 
	fail("PathAndTreeMatcher should not have matched against builder")
		
      case _ => ()
    }
    
  }

  def testNSMatcher : Unit = {
    val Matcher = ns.m
    // Test attribs 
    val a = Attribute(ns("pre","attrib"), "val")
    
    a match {
      case Matcher(at) => 
	assertEquals("val", at.value)
      case _ => fail("Could not match the attribute")
    }

    // test elem
    val e = Elem(ns("localOnly"), emptyAttributes + a)
    
    e match {
      case Matcher(el) => 
	val at = el.attributes(ns("attrib"))
	assertTrue("wasn't present", at.isDefined)
	assertEquals("val", at.get.value)
      case _ => fail("Could not match on the elem")
    }

    val nonMNS = Namespace("uri:nochancefred")
    val NMatcher = nonMNS.m

    a match {
      case NMatcher(at) => 
	fail("Should not have matched "+at)
      case _ => () // good
    }

    e match {
      case NMatcher(el) => 
	fail("Should not have matched "+el)
      case _ => () // good
    }
    
  }

  def testMatcher : Unit = {
    val builder = <(ns { "Elem" })
    val withAttribs = builder /@ (nsa("pre", "attr1") -> "val1", "attr2" -> "val2", nsp { "attr3" } -> "val3")

    val Matcher = ElemMatcher(ns{"Elem"}, nsp{ "attr3" }, "attr2") //"attr", 

    withAttribs.section match {
      case Matcher(elem, attr3 :: attr2 :: Nil) => 
	assertTrue("Matcher matched but did not contain the right qname", elem.name =:= ns{"Elem"})
	assertEquals("val3", attr3.value)
	assertEquals("val2", attr2.value)
	
      case _ => assertFalse("Did not match",true)
    }

    withAttribs.section match {
      case Matcher(elem, Attr(attr3) :: Attr(attr2) :: Nil) => 
	assertTrue("Matcher matched but did not contain the right qname", elem.name =:= ns{"Elem"})
	assertEquals("val3", attr3)
	assertEquals("val2", attr2)
	
      case _ => assertFalse("Did not match",true)
    }

    withAttribs.section match {
      case Matcher(elem, Attr(attr3) :: attr2 :: Nil) => 
	assertTrue("Matcher matched but did not contain the right qname", elem.name =:= ns{"Elem"})
	assertEquals("val3", attr3)
	assertEquals("val2", attr2.value)
	
      case _ => assertFalse("Did not match",true)
    }
    
    withAttribs.section match {
      case Matcher(elem, Attr("val3") :: Attr(attr2) :: Nil) => 
	assertTrue("Matcher matched but did not contain the right qname", elem.name =:= ns{"Elem"})
	assertEquals("val2", attr2)
	
      case _ => assertFalse("Did not match",true)
    }

    // some negatives ...

    withAttribs.section match {
      case Matcher(elem, Attr("shouldnt work") :: Attr(attr2) :: Nil) => 
	assertFalse("Should not have matched, needed both", true)
      case _ => true
    }

    withAttribs.section match {
      case Matcher(elem, Attr("val3") :: Nil) => 
	assertFalse("Should not have matched", true)
      case _ => true
    }

  }

  val builder = <(ns("Elem")) / (ns("Child"), ns("Child2"), ns("Child") /( <(ns("fred")) /( ns("Child") ) )  /(Text("some text")) , ns("Another") )
  val fredi = top(builder).\*(ns("Child")).\*(1)

  def testMoveTo = {

    assertTrue("Could not find fred", fredi.size == 1)

    val childi = top(builder).\*(ns("Child"))

    assertTrue("Could not find child", childi.size == 2)

    val fredpos = fredi.head.position
    val res = moveTo(childi.head, fredpos)

    assertEquals(ns("fred"),res.tree.section.name)
  }

  def testReplace = {

    val folded = foldPositions(fredi)(x=>Replace(ns("SubChild")))
    
    val newRoot = folded.left.get

    val subchildi = newRoot.\*(ns("Child")).\*(1)
    
    assertTrue("Could not find a child below", subchildi.size == 1)
    
    assertEquals(ns("SubChild"),subchildi.head.tree.section.name)
  }

  def testNestedReplace = {
    val parentis = fredi.\^.\\.*
    assertTrue("Did not get 3 nodes, should be child, fred and sub ", parentis.size == 2)

    val folded = foldPositions(parentis ++ fredi)(x=>Replace(ns("HadChildren")))
    //println(folded)
    val newRoot = folded.left.get

    val subchildi = newRoot.\\.*(ns("HadChildren"))
    
    //printTree(newRoot.tree)

    assertTrue("Did not find only one HadChildren", subchildi.size == 1)
  }

  def testRemove = {
    //printTree(builder.toTree)
    val allChild = top(builder).\\.*(ns("Child"))
    //allChild.foreach(x=>println(x.position))
    assertTrue("Could not find all Childs", allChild.size == 3)

    val folded = foldPositions(allChild)(_=>Remove())
    
    val newRoot = folded.left.get
    //printTree(newRoot.tree)

    val nochildi = newRoot.\*(ns("Child"))
    
    assertTrue("Found a Child", nochildi.size == 0)
    
    val otherchildi = newRoot.\*
    
    assertTrue("Should have two other children", otherchildi.size == 2)
    assertEquals(ns("Child2"),otherchildi.head.tree.section.name)
  }

  def testAddBefore = {
    //printTree(builder.toTree)
    val fredChild = top(builder).\\.*(ns("Child")).takeRight(1) 

    val folded = foldPositions(fredChild)(_=>AddBefore( ns("Before") ))
    
    val newRoot = folded.left.get
    //printTree(newRoot.tree)

    val freds = newRoot.\*(ns("Child")).\*(ns("fred")).\*
    
    assertTrue("Fred only has one child", freds.size == 2)
    
    assertEquals(ns("Before"),freds.head.tree.section.name)
    assertEquals(ns("Child"),freds.last.tree.section.name)
  }

  def testAddAfter = {
    //printTree(builder.toTree)
    val fredChild = top(builder).\\.*(ns("Child")).takeRight(1) 

    val folded = foldPositions(fredChild)(_=>AddAfter( ns("After") ))
    
    val newRoot = folded.left.get
    //printTree(newRoot.tree)

    val freds = newRoot.\*(ns("Child")).\*(ns("fred")).\*
    
    assertTrue("Fred only has one child", freds.size == 2)
    
    assertEquals(ns("Child"),freds.head.tree.section.name)
    assertEquals(ns("After"),freds.last.tree.section.name)
  }

  def testCombinedSimpleFolds = {
    val builder = <(ns("i0")) / (ns("i2"), ns("i3"), ns("i40"), ns("i20"), ns("i5"), ns("i7"), ns("i10"), ns("i50"), ns("i11"), ns("i14") )
    //printTree(builder.toTree)
    val all = top(builder).\\.*

    import Elements.Functions.localName 
    assertEquals("i0,i2,i3,i40,i20,i5,i7,i10,i50,i11,i14",all.map(localName(_)).mkString(",") )

    val folded = foldPositions(all)( implicit p =>
      localName match {
	// test inserting before the start
	case "i2" => AddBefore( ns("i1") )
	// replace in the middle
	case "i40" => Replace( ns("i4") )
	// remove in the middle
	case "i20" | "i50" => Remove()
	// after followed by after
	case "i5" => AddAfter( ns("i6") ) 
	// after followed by before
	case "i7" => AddAfter( ns("i8") )
	case "i10" => AddBefore( ns("i9") )
	// after followed by before without any previous after
	case "i11" => AddAfter( ns("i12") )
	case "i14" => AddBefore( ns("i13") )
	// just copy it over - noop
	case _ @ x => AsIs()
      }
    )
    
    //println(folded.right.get)
    val newRoot = folded.left.get
    //printTree(newRoot.tree)

    val res = newRoot.\\.*

    assertEquals("i0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14",res.map(localName(_)).mkString(",") )
    
  }

  def testReplaces = {
    val builder = <(ns("i0")) / (ns("i1"), ns("i30"), ns("i70"), ns("i100"))//, ns("110"))
    //printTree(builder.toTree)

    val all = top(builder).\\.*
    import Elements.Functions.localName 
    assertEquals("i0,i1,i30,i70,i100",all.map(localName(_)).mkString(",") )

    val folded = foldPositions(all)( p =>
      p.tree.section.name.local match {
	case "i30" => Replace(ns("i2"), ns("i3") )
	case "i70" => Replace(ns("i4"), ns("i5") , ns("i6") ,ns("i7") )
	case "i100" => Replace(ns("i8"), ns("i9") , ns("i10") ) 
	// just copy it over - noop
	case _ @ x => AsIs()
      }
    )
    
    //println(folded.right.get)
    val newRoot = folded.left.get
    //printTree(newRoot.tree)

    val res = newRoot.\\.*

    assertEquals("i0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10",res.map(localName(_)).mkString(",") )
    
  }

  // http://stackoverflow.com/questions/4313032/scala-xml-transform-throws-away-transformed-elements-with-multiple-attributes

  def fooIdBits(i : Int) : Stream[XmlTree] = Stream[XmlTree]( <("foo"l)/@("id" -> i.toString) /( ("bar"l)/@("id" -> "0")) /(
      (("baz"l)/@("id" -> "0", "blah" -> "blah", "etc" -> "etc")) /( (("buz"l)/@("id" -> "0")) ),
      (("buz"l)/@("id" -> "0"))
    ) ).append( fooIdBits( i + 1 ) )
 
  val fooIdBuilder = <("root"l) /( fooIdBits(1).take(5) )
    
  def testIdSetting = {
    /* <solution> */
    def toId( id : String )( op : XmlPath ) = 
      foldPositions( top(op.tree).\.\\.*@("id").\^ ){ p => Replace(elem(p) /@("id"-> id) toTree) }
      
    val foos = top(fooIdBuilder).\*
    val folded = foldPositions( foos )( p => 
      ReplaceWith( toId(attributes(p)("id"l).get.value) ) )
    /* </solution> */

    val newRoot = folded.left.get
    val fooos = newRoot.\\.*("foo"l)

    fooos.foreach{ a =>
//println(a)
      val id = attributes(a)("id").get.value

      a.\\.*@("id").foreach(
	i => assertEquals(id, i.value))}
  }

  // http://stackoverflow.com/questions/970675/scala-modifying-nested-elements-in-xml

  def testNestedUpdates = {
    def bits(i : Int) : Stream[XmlTree] = Stream[XmlTree]( <("subnode"l) ~> i.toString ).append( Stream[XmlTree](<("contents"l) ~> i.toString toTree) ).append( bits( i + 1 ) )

    val builder = <("root"l) /( bits(0).take(6) )
    
    import Elements.Functions.normalizeSpace

    /* <solution> although I'm also adding not just setting to "2" for testing purposes */
    val subnodes = top(builder).\\.*("subnode"l)
    val folded = foldPositions( subnodes )( p => 
      Replace( p.tree ~> ((normalizeSpace( p ).toInt + 1).toString) ))// toTree ) )
    /* </solution> */

    val newRoot = folded.left.get

    val contents = newRoot.\*("contents"l)

    contents.foreach{ a =>
      val v = normalizeSpace(a).toInt

      assertEquals( (v + 1).toString, normalizeSpace(a.previousSibling))}
  }

  // variation on id setting but with combination of the operations
  def testIdAndBlahAndFailOnNoPathsSetting = {
    def toId( id : String )( op : XmlPath ) =
      foldPositions( top(op.tree).\.\\.*@("id").\^ ){ p => Replace(p.tree.section /@("id"-> id) toTree) }

    val toBlahs = ( op : XmlPath ) =>
      foldPositions( top(op.tree).\.\\.*@("blah").\^ ){ p => Replace(p.tree.section /@("blah"-> "blahs") toTree) }

    val willFail = ( op : XmlPath ) =>
      foldPositions( top(op.tree).\.\\.*@("orange_raspberries").\^ ){ p => AsIs() }

    val clearFail = ( p : XmlPath ) => Right(NoPaths)

    val foos = top(fooIdBuilder).\*
    val folded = foldPositions( foos )( p => 
      ReplaceWith( willFail | clearFail | toBlahs & toId( p.tree.section.attributes("id"l).get.value) ) )
    
    assertTrue("Was not left, was " + folded, folded.isLeft)
    val newRoot = folded.left.get

    val fooos = newRoot.\\.*("foo" localOnly)

    fooos.foreach{ a =>
      val id = a.tree.section.attributes("id").get.value

      a.\\.*@("id").foreach(
	i => assertEquals(id, i.value))

      a.\\.*@("blah").foreach(
	i => assertEquals("blahs", i.value))}
  }

  def testSimpleConvert = {
    val orig = <root><child/></root>
    val converted = orig.asScales
    assertEquals("root", converted.rootElem.section.name.local)
  }

  def testEmbedElems = {
    val unprefixedQName = "uri:namespace" :: "localName"
    val elem = Elem(unprefixedQName)
    val pre = """<?xml version="1.0" encoding="UTF-8"?>"""
    val stub = """<localName"""
    val xmlns = """ xmlns="uri:namespace""""
    assertEquals(pre+stub+xmlns+"/>",asString(elem))
    
    val root = <(elem) /( elem )
    
    assertEquals(pre+stub+xmlns+">"+stub+"/></localName>", asString(root))
  }

  val builderFold = <(ns("i0")) / (ns("i2"), ns("i3"), ns("i40"), ns("i20"), ns("i5"), ns("i7"), ns("i10"), ns("i50"), ns("i11"), ns("i14") )

  def testBuilderFold : Unit = {
    val b = builderFold.fold{ _.\*(ns("i40")) }{
      p => Replace(p.tree.section /@("blah"-> "blahs") toTree)
    }
    
    assertTrue("should have been left", b.isLeft)
    
    val b1 = top(b.left.get).\\@("blah"l).\^.*(ns("i40"))

    assertTrue("i40 should have contained blah as an attribute", 
	       boolean( b1 ) )
  }

  def testBuilderFold_! : Unit = {
    try {
      builderFold.fold_!( _.\*("fred"l)) {
	p => AsIs()
      }
      fail("shouldn't get further")
    } catch {
      case FoldErrorException(NoPaths) => () // ok 
      case e : Throwable => fail("Not expecting this exception " + e)
    }
  }

  def testBuilderFold_? : Unit = {
    val b = builderFold.fold_?( _.\*("fred"l)) {
      p => AsIs()
    }
    assertEquals(builderFold, b)
  }

  def testBuilderThrowFold_? : Unit = {
    try {
      builderFold.fold_?( x => x : XPath[List[XmlPath]] ) {
	p => Remove()
      }
      fail("shouldn't get further")
    } catch {
      case FoldErrorException(RemovedRoot) => () // ok 
      case e : Throwable => fail("Not expecting this exception " + e)
    }
  }

}
  
