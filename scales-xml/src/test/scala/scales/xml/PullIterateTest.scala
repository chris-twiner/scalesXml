package scales.xml

import scalaz.EphemeralStream

/**
 * Prior to 0.4.4 iterate was based on onQNames, the PullTest qnames test covered this, and although the implementation is pretty much identical, these test from the iterate function directly.
 */ 
class PullIterateTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  val p = new PullTest
  import p._
/* */
  def testOnQNamesLastElement : Unit = {    
    // ourMax of 9000000 and WeakStream processess in 512s without issue
    // It truly seems something is different here, with the others 
    // we won't get past iterator...
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = EphemeralStream.toIterable(eevents(ourMax))

    val QNames = List("root"l, "child"l, "interesting"l)

    val itr = iterate(QNames, iter.iterator)

    // skip the first
    
    def and1( i : Int) : Stream[Int] = 
      Stream.cons(i, 
	Stream.cons(i + 1, and1(i+1)))

    for{
      (x,i) <- itr.zip(and1(1).iterator)
    } {
      assertEquals( "interesting content "+ i, text(x))
      assertEquals(1, x.zipUp.children.size)
    }
  }

  def testOnQNamesManyElementsBelow : Unit = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = events(ourMax).iterator
    
    val QNames = List("root"l, "child"l)
    
    val itr = iterate(QNames, iter)
    
    for {
      (x, i) <- itr.zipWithIndex
    } {
      assertEquals("Wasn't giving back child", "{}child", qualifiedName(x))
      assertEquals( "interesting content "+ (i+1) +"interesting content "+ (i + 2)
		   , text(x))
      val count = x.zipUp.children.size
      if (count != 1){
	x.zipUp.children.foreach{x => printTree(x.getRight);println()}
	printTree(rootPath(x).tree)
	fail("more than one " + count +" at "+ println(elem(x)))
      }
    }
    
  }

  def testOnQNamesRepeatedQNames = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = events(ourMax).iterator
    
    val itr = iterate(repeatingQNames, iter)

    for { 
      (x, i) <- itr.zipWithIndex
    } {
      // we want to see both sub text nodes
      assertEquals( "content "+ (i + 2)
		   , text(x))
      assertEquals(1, x.zipUp.children.size)
    }
    
  }

}
