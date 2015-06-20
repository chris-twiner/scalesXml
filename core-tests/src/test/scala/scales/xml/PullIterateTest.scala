package scales.xml

import ScalesXml._
import scalaz._
import scales.utils.ScalesUtils._

object PullTestHelpers {
  sealed trait WeakStream[+A] {

    val empty : Boolean

//    def ++( e : => WeakStream[A]) : WeakStream[A] = WeakStream.append(this, e)
    def ++[B >: A]( e : => WeakStream[B]) : WeakStream[B] = WeakStream.append[A, B](this, e)
    
    def head : () => A
    def tail : () => WeakStream[A]    
  }

  /**
   * Heavily borrowed from scalaz Ephemeral but I really like my stack and ++
   * If I get it working I'll ask to patch..
   */ 
  object WeakStream {

    def empty[A] = new WeakStream[A]{
      val empty = true
      def head = error("empty")
      def tail = error("empty")
    }

    object cons {
      def apply[A](a: => A, as: => WeakStream[A]) = new WeakStream[A] {
	val empty = false
	val head = weakMemo(a)
	val tail = weakMemo(as)
      }
    }

    def apply[A](a : A, as : A *) : WeakStream[A] = new WeakStream[A]{
      val empty = false
      val head = weakMemo(a)
      def tail = weakMemo{
	if (as.isEmpty) WeakStream.empty
	else {
	  val astail = as.tail;
	  cons(as.head, if (astail.isEmpty) WeakStream.empty else
	    apply(astail.head, 
	      astail.tail :_*))}}
    }

    implicit def toIterable[A](e: WeakStream[A]): Iterable[A] = new Iterable[A] {
      def iterator = new Iterator[A] {
	var cur = e
	def next = {
          val t = cur.head()
          cur = cur.tail()
          t
	}
	def hasNext = !cur.empty
      }
    }

    def append[A, B >: A]( a : WeakStream[A], e : => WeakStream[B] ) : WeakStream[B] = 
      if (!a.empty) cons(a.head(), append(a.tail(), e))
      else e

    def weakMemo[V](f: => V): () => V = {
      import java.lang.ref.{WeakReference, SoftReference}
    
      val latch = new Object
      @volatile var v: Option[WeakReference[V]] = None
      () => {
	val a = v.map(x => x.get)
	if (a.isDefined && a.get != null) a.get else latch.synchronized {
          val x = f
          v = Some(new WeakReference(x))
          x
	}
      }
    }
  }

  def childEvents(i : Int, max : Int) : WeakStream[PullType] = 
    childEvents(i, max, childEvents(i + 1, max))
  
  def childEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] = {

    def ichildEvents(i : Int, max : Int) : WeakStream[PullType] = 
    if (i < max) {
       WeakStream[PullType](
        Elem("unloved"l),
          Text("unloved content"),
        EndElem("unloved"l),
        Elem("child"l),
          Elem("interesting"l),
            Text("interesting content "+i),
          EndElem("interesting"l),
          Elem("interesting"l),
	    Text("interesting "),
            Elem("interesting"l),
              Text("content "+(i + 1)),
	    EndElem("interesting"l),
          EndElem("interesting"l),
        EndElem("child"l)
      ) ++ next
    }
    else WeakStream.empty[PullType]

    ichildEvents(i, max)
  }
  def events(children : => WeakStream[PullType]) : WeakStream[PullType] = 
    WeakStream[PullType](Left(Elem("root"l))) ++ (children) ++ (WeakStream[PullType](Right(EndElem("root"l))))

  def events(max : Int) : WeakStream[PullType] = 
    events(childEvents(1 ,max))

  def echildEvents(i : Int, max : Int) : EphemeralStream[PullType] =
    echildEvents(i, max, echildEvents(i + 1, max))

  def echildEvents(i : Int, max : Int, next : => EphemeralStream[PullType]) : EphemeralStream[PullType] = {

    def ichildEvents(i : Int, max : Int) : EphemeralStream[PullType] = 
    if (i < max) {
      EphemeralStream[PullType](
        Left(Elem("unloved"l)),
          Left(Text("unloved content")),
        Right(EndElem("unloved"l)),
        Left(Elem("child"l)),
          Left(Elem("interesting"l)),
            Left(Text("interesting content "+i)),
          Right(EndElem("interesting"l)),
          Left(Elem("interesting"l)),
	    Left(Text("interesting ")),
            Left(Elem("interesting"l)),
              Left(Text("content "+(i + 1))),
	    Right(EndElem("interesting"l)),
          Right(EndElem("interesting"l)),
        Right(EndElem("child"l))
      ) +:+ next
    }
    else EphemeralStream[PullType]()

    ichildEvents(i, max)
  }
  def eevents(children : => EphemeralStream[PullType]) : EphemeralStream[PullType] = 
    EphemeralStream[PullType](Left(Elem("root"l))) ++ (children) ++ (EphemeralStream[PullType](Right(EndElem("root"l))))

  def eevents(max : Int) : EphemeralStream[PullType] = 
    eevents(echildEvents(1 ,max))
  
/* Stream blows the stack */

  def schildEvents(i : Int, max : Int) : Stream[PullType] =
    schildEvents(i, max, schildEvents(i + 1, max))

  def schildEvents(i : Int, max : Int, next : => Stream[PullType]) : Stream[PullType] = {

    def ichildEvents(i : Int, max : Int) : Stream[PullType] = 
    if (i < max) {
      Stream[PullType](
        Left(Elem("unloved"l)),
          Left(Text("unloved content")),
        Right(EndElem("unloved"l)),
        Left(Elem("child"l)),
          Left(Elem("interesting"l)),
            Left(Text("interesting content "+i)),
          Right(EndElem("interesting"l)),
          Left(Elem("interesting"l)),
	    Left(Text("interesting ")),
            Left(Elem("interesting"l)),
              Left(Text("content "+(i + 1))),
	    Right(EndElem("interesting"l)),
          Right(EndElem("interesting"l)),
        Right(EndElem("child"l))
      ) ++ next
    }
    else Stream[PullType]()

    ichildEvents(i, max)
  }
  def sevents(children : => Stream[PullType]) : Stream[PullType] = 
    Stream[PullType](Left(Elem("root"l))) ++ (children) ++ (Stream[PullType](Right(EndElem("root"l))))

  def sevents(max : Int) : Stream[PullType] = 
    sevents(schildEvents(1 ,max))

  def extraChildEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] = 
    if (i < max)
      WeakStream[PullType](
        Elem("anotherChild"l),
          Elem("stillInteresting"l),
            Text("interesting content "+i),
          EndElem("stillInteresting"l),
        EndElem("anotherChild"l)
      ) ++ next 
    else WeakStream.empty[PullType]

  val repeatingQNames = List("root"l, "child"l, "interesting"l, "interesting"l)
  val stillInterestingQNames = List( "root"l, "anotherChild"l, "stillInteresting"l )

}

/**
 * Prior to 0.4.4 iterate was based on onQNames, the PullTest qnames test covered this, and although the implementation is pretty much identical, these test from the iterate function directly.
 */ 
class PullIterateTest extends junit.framework.TestCase {

  import PullTestHelpers._

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  import Functions._

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

    val iter = EphemeralStream.toIterable(eevents(ourMax)).iterator

    val QNames = List("root"l, "child"l, "interesting"l)

    val itr = iterate(QNames, iter)

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

  def testOnQNameEqualImplicit : Unit = {
    
    import scalaz.Equal._, scalaz._, scalaz.Scalaz._

    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = events(ourMax).iterator

    implicit val qnameEqual = equal { (a: QName, b: QName) =>
      a.local.equalsIgnoreCase(b.local)
    }
    
    val QNames = List("root"l, "CHILD"l, "interESTING"l)
    
    val itr = iterateI(QNames, iter)

    assertFalse("failed to match nodes", itr.isEmpty)

  }

}
