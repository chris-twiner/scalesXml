package scales.xml

class DslBuildersTest29 extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import ScalesUtils._

  import ScalesXml._

  import Functions._

  val ns = Namespace("test:uri")
  val nsa = Namespace("test:uri:attribs")
  val nsp = nsa.prefixed("pre")
    
  import scales.xml.equals._
  import scalaz._
  import Scalaz._

  def testIssue2_ReplaceWith_Nested : Unit = {
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
    
    // for every child element add a text child that contains the qname of the elem
    def addTextNodes( op : XmlPath ) = {
      foldPositions( op.\* ) { 
	p => Replace( p.tree / qname(p) ) 
      }
    }

    val allReplaced = addTextNodes( top(builder) )

    val allReplacedExpected = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child") ~> "Child",
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text", "Child2" )
		     )

    assertTrue("allReplaced Was not equal", allReplaced.left.get.tree === allReplacedExpected)

    val nodes = top(builder). \\*(ns("Child2"))

    // this should be the same logically
    val direct = addTextNodes( nodes.head )

    val res = foldPositions( nodes  ){
      _ => ReplaceWith(addTextNodes _)
    }

//    println(asString(res.left.get.tree))

    val resExpected = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") /("text","Subchild") )
		     )
    
    assertTrue("direct Was not equal", direct.left.get.tree === resExpected)
    assertTrue("res Was not equal", res.left.get.tree === resExpected)
  }

  def testReplaceWithRemoval : Unit = {

    val ns = Namespace("test:uri")
    val nsa = Namespace("test:uri:attribs")
    val nsp = nsa.prefixed("pre")

    val builder = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )

    // for every child element add a text child that contains the qname of the elem
    def removeEmptyNodes( op : XmlPath ) = {
      foldPositions( op.* ) { // filter for elems only
	case p if (!p.hasChildren) => 
	  Remove()
	case _ => AsIs()
      }
    }

    val singleReplaced = removeEmptyNodes( top(builder).\*.head )

    val singleReplacedExpected = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )

    assertTrue("allReplaced Was not equal", singleReplaced.left.get.tree === singleReplacedExpected)

    val nodes = top(builder).\\*

    val res = foldPositions( nodes  ){
      p => ReplaceWith(removeEmptyNodes _)
    }

//    println(asString(res.left.get.tree))

    val allReplacedExpected = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text")
		     )

    assertTrue("allReplaced Was not equal", res.left.get.tree === allReplacedExpected)
  }

  def testReplaceWithRemovalWholeTreeError : Unit = {

    val ns = Namespace("test:uri")
    val nsa = Namespace("test:uri:attribs")
    val nsp = nsa.prefixed("pre")

    val builder = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )

    // for every child element add a text child that contains the qname of the elem
    def removeEmptyNodes( op : XmlPath ) = {
      foldPositions( op.* ) { // filter for elems only
	p => Remove()
      }
    }

    val singleReplaced = removeEmptyNodes( top(builder).\*.head )

    val singleReplacedExpected = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )

    assertTrue("allReplaced Was not equal", singleReplaced.left.get.tree === singleReplacedExpected)

    val nodes = top(builder).\\*

    val res = foldPositions( nodes  ){
      p => ReplaceWith(removeEmptyNodes _, wholeTree = true)
    }

    assertTrue("res should have been a failure", res.isRight)
    assertTrue("should have been removed root "+res.right.get, res.right.get eq RemovedRoot)

  }


  def testMultipleRoots : Unit = {

    val ns = Namespace("test:uri")
    val nsa = Namespace("test:uri:attribs")
    val nsp = nsa.prefixed("pre")

    val builder1 = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )

    val builder2 = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content")

    val all = top(builder1).\\* | top(builder2).\\*

    val res = foldPositions( all ) { // filter for elems only
	case p if (!p.hasChildren) => 
	  Remove()
	case _ => AsIs()
      }

    assertTrue("Should be an error", res.isRight)
    assertTrue("Should have been NoSingleRoot was "+res.right.get, 
	       res.right.get eq NoSingleRoot)

   }

  def testBeforeOrAfterRoot : Unit = {
    val ns = Namespace("test:uri")
    val nsa = Namespace("test:uri:attribs")
    val nsp = nsa.prefixed("pre")

    val builder1 = 
      ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	     "attr2" -> "val2",
		     nsp("attr3") -> "val3") /(
		       ns("Child"),
		       "Mixed Content",
		       ns("Child2") /( ns("Subchild") ~> "text",
				    ns("EmptySub"))
		     )
 
    val res = foldPositions( top(builder1).\\* ) { // filter for elems only
	p => AddBefore("text")
      }

    assertTrue("Should be an error", res.isRight)
    assertTrue("Should have been AddedBeforeOrAfterRoot was "+res.right.get, 
	       res.right.get eq AddedBeforeOrAfterRoot)
  }

  def testOptionalAttrib : Unit = {
    val ns = Namespace("test:uri")

    val emptyRoot = <(ns("Elem"))
    
    val builder = 
      ns("Elem") /@ None
    
    assertTrue( emptyRoot === builder )
  }

  def testIterableAttribs : Unit = {
    val ns = Namespace("test:uri")

    val emptyRoot = <(ns("Elem"))
    
    val builder = 
      ns("Elem") /@( List() )
    
    assertTrue( emptyRoot === builder )  

    val builder2 = 
      ns("Elem") /@( "attr2" -> "val2" )
    val builder3 = 
      ns("Elem") /@( one[Attribute]("attr2" -> "val2") )
    
    assertTrue( builder2 === builder3 )
  }

  def testOptionalText : Unit = {
    val ns = Namespace("test:uri")

    val emptyRoot = <(ns("Elem"))
    
    val builder = 
      ns("Elem") ~> None
    val builder2 = 
      ns("Elem").setValue( None )
    
    assertTrue( emptyRoot === builder )
    assertTrue( emptyRoot === builder2 )
  }

  def testNoneDestructiveText : Unit = {
    val ns = Namespace("test:uri")

    val hasKids = 
      ns("Elem") ~> "fred"

    val noneKid = hasKids ~> None
    
    assertTrue( hasKids === noneKid )
  }

  def testOptionalChild : Unit = {
    val ns = Namespace("test:uri")

    val hasKids = 
      ns("Elem") /( ns("elem"), ns("elem") )

    val additional = hasKids /( Some[ItemOrElem](ns("additional")) )

    val wholeFamily = 
      ns("Elem") /( ns("elem"), ns("elem"), ns("additional") )

    assertTrue( additional === wholeFamily )
    
    val nonDestructive = wholeFamily /( None )

    assertTrue( additional === nonDestructive )
  }

}
  
