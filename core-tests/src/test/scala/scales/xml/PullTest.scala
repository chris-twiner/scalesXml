package scales.xml

class PullTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import io._
  import ScalesUtils._
  import ScalesXml._

  import Functions._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import scalaz._
  import Scalaz._
  import scalaz.IterV._

  import scales.utils.{resource => sresource}
 
  /*
   * pump up number when cold, but make sure its even.
   *
   * 5000000 takes around 185s (as of 14.01.2010) and shows no leaking/unneccesary retention.
   */
  val maxIterations = 500//0000 

  def testSimpleLoad = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", pull.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)
    
    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok 
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    pull.close
    assertTrue("Should have been closed", pull.isClosed)
  }

  def testResourceLoad = {
    val (resource, pull) = pullXmlResource(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", resource.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)
    
    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok 
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    resource.closeResource
    assertTrue("Should have been closed", resource.isClosed)
  }

  def testProxiedCloser = {
    val strr = new java.io.StringReader("My String")
    val proxied = new ProxiedCloseOnNeedReader(strr)

    proxied.close() // should be a noop
    assertFalse("Was closed for some reason", proxied.isClosed)

    var arr = Array[Char](10)

    val count = proxied.read(arr)
    assertEquals("My String".substring(0,count),arr.mkString(""))

    proxied.closeResource
    assertTrue("Should have been closed", proxied.isClosed)  
  }

  def testChainedCloseOnNeed = {
    import java.io.StringReader
    def getOne = new StringReader("My String") with CloseOnNeed {
      def doClose() {
	super[StringReader].close()
      }
    }
    val str1 = getOne
    val str2 = getOne
    val str3 = getOne
    
    val joined = str1 ++ str2 ++ str3
    assertFalse("1 Should not have been closed", str1.isClosed)  
    assertFalse("2 Should not have been closed", str2.isClosed)  
    assertFalse("3 Should not have been closed", str3.isClosed)  
    assertFalse("Joined Should not have been closed", joined.isClosed)  

    joined.closeResource

    assertTrue("1 Should have been closed", str1.isClosed)  
    assertTrue("2 Should have been closed", str2.isClosed)  
    assertTrue("3 Should have been closed", str3.isClosed)  
    assertTrue("Joined Should have been closed", joined.isClosed)    
  }

  def testDump = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))

    def out(it : String) : Unit = 
      ()//it
    
    for{event <- pull}{
      event match {
	case Left(x) => x match {
	  case Elem(qname, attrs, ns) =>
	    out("<" + qname + attrs.map( x => " "+x.name +"='"+x.value+"'" ).mkString(" ") + ">")

	  case item : XmlItem =>
	    out("item "+item)
	} 
	case Right(EndElem(qname, ns)) =>
	  out("</"+ qname +">")
	
      }
    }
    
    assertTrue("Should have been closed", pull.isClosed)
  }

  def testDropWhileIterateeOwnEnum = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))

    val isShouldRedeclare = (x : PullType) => x match {
      case Left(Elem(qname, _, _) ) if qname.local == "ShouldRedeclare" =>
	false
      case _ => true
    }
    val isEndDefault = (x : PullType) => x match {
      case Right(EndElem(qname, _) ) if qname.local == "Default" =>
	false
      case _ => true
    }

    val iteratee = dropWhile[PullType]( isShouldRedeclare )

    val res = iteratee(pull.it) run

    assertTrue("Should have been some", res.isDefined)
    //println(res.get)
    assertTrue("Should have been a ShouldRedeclare", !isShouldRedeclare(res.get))

    // sanity check, we should now have an endelem
    val sanity = head(pull.it) run

    def sanityCheck = {

      assertTrue("Should have been some ShouldDeclare end", sanity.isDefined)
      assertTrue("Should have been an Element end "+sanity.get, sanity.get.isRight)
      assertTrue("Should have been a ShouldDeclare end", sanity.get.right.get.name.local == "ShouldRedeclare")
    }

    sanityCheck
    // check again, only works if we cache the values, which is possibly bad for memory usage
    sanityCheck

    val iteratee2 = dropWhile[PullType]( isEndDefault )//(pull).run
    val res2 = iteratee2(pull.it) run

    assertTrue("Should have been some - Default end", res2.isDefined)
    assertTrue("Should have been a Default end", !isEndDefault(res2.get))

    assertFalse("should have no next", pull.hasNext)
    assertTrue("Should have been closed", pull.isClosed)
  }

  /**
   * this test relies on the default memory of the given system, no way to test from within.
   * It also tests from outside in through the stax interface itself, hence the rather unpleasent acrobatics below.
   */ 
  def testConstantSpace = {
    
    //5 events each chunk
    val out = "<root><sub><dummy></dummy></sub>"

    val reader = new java.io.PushbackReader( new java.io.StringReader(out), 4000 )

    val itr = evalWith( (x: Int) => {
      if ((x % 5 == 0) && x < maxIterations) {
	  val xml = "<sub><child>"+x+"</child></sub>"

	  reader.unread(xml.toCharArray())
	}
	if (x == maxIterations) {
	  // get the rest of the reader and add the root to it
	  var res = 0;
	  val read = new java.lang.StringBuilder
	  try{
	  while( res != -1 ) {
	    res = reader.read
	    if (res != -1)
	      read.append(res)
	  }
	  } catch {
	    case _ : Throwable => println(read.toString)
	  }
	  
	  reader.unread(read.append("</root>").toString.toCharArray());
	}
	x
      })

    val count = (1 to maxIterations).iterator
    itr(count) eval

    val pull = pullXml(reader)

    val isEndRoot = (x : PullType) => {
      // pump more events
      itr(count).eval

      x match {
      case Right(EndElem(qname, _) ) if qname.local == "root" =>
	false
      case _ => 
	true
      
    }}
    
    val iteratee2 = dropWhile[PullType]( isEndRoot )
    val res2 = iteratee2(pull.it) run

    assertTrue("Should have been some - root end", res2.isDefined)
    assertTrue("Should have been a root end", !isEndRoot(res2.get))

    assertFalse("should have no next", pull.hasNext)
    assertTrue("Should have been closed", pull.isClosed)

  }

  /*

There are three main use cases to do with how the pull logic should combine.

<root><many>... <deep><deeper><deepest></deepest></deeper></deep>
<others><of><interest>...
...</many></root>

1) Only elements matching a given path should be taken:

ie. take only deepests that are below <deep><deeper>

2) Take elements matching a path but don't exhaust them

same example above but after processing all deepests we want the <interest>s as well

3) Take alternating results

Here there are repetitions of <deep>..<others><deep><others...

and we want to always match them correctly

The barrage of tests below test the resumable IterV version, onDone resumable and then finally a chunk of xml tests
on both the qname matching (3 of them) and then the above combos

   */ 

  def testResumableIterConversion = {
    val liter = (1 to maxIterations).iterator
    
    type ITER = ResumableIter[Int, Option[Int]]

    val itrOdd : ITER  = find( (x : Int) => x % 2 == 1 )

    def isDone( i : Int, res : ITER) = 
      res match {
	case Done((x,cont), y) => 
	  assertTrue("is defined "+i, x.isDefined)
	  assertEquals(i, x.get)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case _ => fail("was not done "+i)
      }

    var res = itrOdd(liter).eval
    isDone(1, res)

    // check it does not blow up the stack and/or mem

    (2 to maxIterations).iterator.filter(_ % 2 == 1).foreach { i =>
      res = extractCont(res)(liter).eval
      isDone(i, res)
    }

    res = extractCont(res)(liter).eval
    res match {
      case Done((x,cont), y) => 
	assertFalse("should not be defined", x.isDefined)
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done")
    }
  }

  /**
   * Normal iters can't maintain state if they return Done, since
   * we pass back a new iter as well we can keep state
   */ 
  def testResumableIterFolds = {
    val liter = (1 to maxIterations).iterator
    
    type ITER = ResumableIter[Int, Long]

    val counter = runningCount[Int] 

    def isDone( i : Int, res : ITER) = 
      res match {
	case Done((x,cont), y) => 
	  assertEquals(i, x)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case _ => fail("was not done "+i)
      }

    var res = counter(liter).eval
    isDone(1, res)

    // check it does not blow up the stack and/or mem

    (2 to maxIterations).iterator.foreach { i =>
      res = extractCont(res)(liter).eval
      isDone(i, res)
    }

    res = extractCont(res)(liter).eval
    res match {
      case Done((x,cont), y) => 
	assertEquals(maxIterations, x)
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done")
    }
  }

  /**
   * Test two resumables next to each other, there should always be one done, the count
   * and another that is done only each three.
   */ 
  def testResumableOnDone = {
    val liter = (1 to maxIterations).iterator
    
    val counter = runningCount[Int] 

    def step( list : List[Long])( s : Input[Int] ) : ResumableIter[Int,Long] =
      s(el = {e =>
	val next = e.longValue :: list
	if (next.size == 3)
	  Done((e, Cont(step(List()))), IterV.Empty[Int])
	else
	  Cont(step(next))
	},
        empty = Cont(step(list)),
        eof = Done((list.last, Cont(step(List()))),IterV.EOF[Int]))
    
    val inThrees = Cont(step(List()))      
    
    val ionDone = onDone(List(counter, inThrees))

    def isDone( i : Int, res : ResumableIterList[Int,Long]) = 
      res match {
	case Done((x :: Nil,cont), y) if i % 3 != 0 => 
	  assertEquals(i, x)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done((x :: x2 :: Nil,cont), y)  if i % 3 == 0 => 
	  assertEquals(i, x)
	  assertEquals(i, x2)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done(x,y) => fail("Was done but not expected "+ x +" -> " + y )
	case _ => fail("was not done "+i)
      }

    var res = ionDone(liter).eval
    isDone(1, res)

    // check it does not blow up the stack and/or mem

    (2 to maxIterations).iterator.foreach { i =>
      res = extractCont(res)(liter).eval
      isDone(i, res)
    }

    res = extractCont(res)(liter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }

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

  
  sealed trait WeakStream[+A] {

    val empty : Boolean

//    def ++( e : => WeakStream[A]) : WeakStream[A] = WeakStream.append(this, e)
    def ++[B >: A]( e : => WeakStream[B]) : WeakStream[B] = WeakStream.append[A, B](this, e)
    
    def head : () => A
    def tail : () => WeakStream[A]    
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

  def testOnQNamesLastElement = {    
    // ourMax of 9000000 and WeakStream processess in 512s without issue
    // It truly seems something is different here, with the others 
    // we won't get past iterator...
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    var at = -1

try{

    val iter = eevents(ourMax).iterator
at=0
    val QNames = List("root"l, "child"l, "interesting"l)

    val ionDone = onDone(List(onQNames(QNames)))

    def isDone( i : Int, res : ResumableIterList[PullType,QNamesMatch]) = 
      res match {
	case Done(((QNames, Some(x)) :: Nil,cont), y)  => 
	  assertEquals( "interesting content "+ i, text(x))
	  assertEquals(1, x.zipUp.children.size)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done((list, cont), y) => 
	  fail("was done with "+ i+" "+list+" and input "+ y +" iter hasNext == "+iter.hasNext)
	case _ => fail("was not done "+i+" was " + res )
      }

    var res = ionDone(iter).eval
    isDone(1, res)
    res = extractCont(res)(iter).eval
    isDone(1+1, res)
    // check it does not blow up the stack and/or mem
at=2
    (2 to (ourMax - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isDone(i, res)
      res = extractCont(res)(iter).eval
      isDone(i + 1, res)
at += 1
    }

    res = extractCont(res)(iter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }
} catch {
  case e : StackOverflowError => println("got to " + at)
}
  }

  def testOnQNamesManyElementsBelow = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = events(ourMax).iterator
    
    val QNames = List("root"l, "child"l)
    val ionDone = onDone(List(onQNames(QNames)))

    def isDone( i : Int, res : ResumableIterList[PullType,QNamesMatch]) = 
      res match {
	case Done(((QNames, Some(x)) :: Nil,cont), y)  =>
	  // we want to see both sub text nodes
	  assertEquals( "interesting content "+ i +"interesting content "+ (i + 1)
		       , text(x))
	  val count = x.zipUp.children.size
	  if (count != 1){
	    x.zipUp.children.foreach{x => println(x);println()}
	    printTree(rootPath(x).tree)
	    fail("more than one " + count +" at "+ println(elem(x)))
	  }
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done((list, cont), y) => 
	  fail("was done with "+ i+" "+list+" and input "+ y +" iter hasNext == "+iter.hasNext)
	case _ => fail("was not done "+i+" was " + res )
      }

    var res = ionDone(iter).eval
    isDone(1, res)
    // check it does not blow up the stack and/or mem

    (2 to (ourMax - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isDone(i, res)
    }

    res = extractCont(res)(iter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }
    
  }

  def testOnQNamesRepeatedQNames = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    val iter = events(ourMax).iterator
    
    val ionDone = onDone(List(onQNames(repeatingQNames)))

    def isDone( i : Int, res : ResumableIterList[PullType,QNamesMatch]) = 
      res match {
	case Done(((repeatingQNames, Some(x)) :: Nil,cont), y)  =>
	  // we want to see both sub text nodes
	  assertEquals( "content "+ (i + 1)
		       , text(x))
	  assertEquals(1, x.zipUp.children.size)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done((list, cont), y) => 
	  fail("was done with "+ i+" "+list+" and input "+ y +" iter hasNext == "+iter.hasNext)
	case _ => fail("was not done "+i+" was " + res )
      }

    var res = ionDone(iter).eval
    isDone(1, res)
    // check it does not blow up the stack and/or mem

    (2 to (ourMax - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isDone(i, res)
    }

    res = extractCont(res)(iter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }
    
  }

  def testAlternating = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    def alternate( i : Int, max : Int) : WeakStream[PullType] = 
      extraChildEvents(i, ourMax, childEvents(i, ourMax, 
		       alternate(i+1, max) ))

    def alternating = events( alternate(1, ourMax) )

    //alternating.foreach(println)

    val iter = alternating.iterator

    var res = altOnDone(iter).eval
    isInteresting(1, res)
    res = extractCont(res)(iter).eval
    isContent(2, res)
    // check it does not blow up the stack and/or mem

    (2 to (ourMax - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isInteresting(i, res)
      res = extractCont(res)(iter).eval
      isContent(i+1, res)
    }

    res = extractCont(res)(iter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }
    
  }

  val repeatingQNames = List("root"l, "child"l, "interesting"l, "interesting"l)
  val stillInterestingQNames = List( "root"l, "anotherChild"l, "stillInteresting"l )

  val altOnDone = onDone(List(onQNames(repeatingQNames), 
			      onQNames(stillInterestingQNames)))

  val (isInteresting, isContent) = {
  
    def isDone(content : String, QNames : List[QName])( i : Int, res : ResumableIterList[PullType,QNamesMatch]) = 
      res match {
	case Done(((QNames, Some(x)) :: Nil,cont), y)  =>
	  // we want to see both sub text nodes
	  assertEquals( content+" "+ i
		       , text(x))
	  assertEquals(1, x.zipUp.children.size)
	  assertTrue("should have been Empty "+i, isEmpty(res))
	case Done((list, cont), y) => 
	  fail("was "+content+" done with "+ i+" "+list+" and input "+ y)
	case _ => fail("was not "+content+" done "+i+" was " + res )
      }
    
    (isDone("interesting content", stillInterestingQNames) _,
      isDone("content", repeatingQNames) _)
  }

  def testFirstThenNext = {
    
    val ourMax = maxIterations / 10 // full takes too long but does work in constant space

    def extraChildren( i : Int, max : Int) : WeakStream[PullType] =
      extraChildEvents( i, max, extraChildren( i+1 , max ))

    def alternating = events( childEvents(1, ourMax/2) ++ (
      extraChildren(1, ourMax/2) ))

    //alternating.foreach(println)

    val iter = alternating.iterator

    var res = altOnDone(iter).eval
    isContent(2, res)
    // check it does not blow up the stack and/or mem

    (2 to (ourMax/2 - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isContent(i+1, res)
    }

    (1 to (ourMax/2 - 1)).iterator.foreach { i =>
      res = extractCont(res)(iter).eval
      isInteresting(i, res)
    }

    res = extractCont(res)(iter).eval
    res match {
      case Done((Nil,cont), y)  => 
	assertTrue("should have been EOL", isEOF(res))
      case _ => fail("was not done with empty")
    }
    
  }


  def ofInterestEvents(i : Int, max : Int) : WeakStream[PullType] = {
    def iOfInterestEvents( i : Int, max :Int) : WeakStream[PullType] =
    if (i < max)
      WeakStream[PullType](
          Left(Elem("ofInterest"l)),
	    Left(Elem("value"l)),
              Left(Text((i + 1).toString)),
	    Right(EndElem("value"l)),
          Right(EndElem("ofInterest"l))
      ) ++ iOfInterestEvents(i + 1, max)
    else WeakStream.empty[PullType]

    iOfInterestEvents(i, max)
  }

  def sectionEvents(i : Int, max : Int, next : => WeakStream[PullType]) : WeakStream[PullType] = 
    if (i < max)
      WeakStream[PullType](
        Left(Elem("section"l)),
          Left(Elem("sectionHeader"l)),
            Left(Text(i.toString)),
          Right(EndElem("sectionHeader"l))        
      ) ++ (ofInterestEvents(0, i)) ++ (WeakStream[PullType](Right(EndElem("section"l)))) ++ ( next )
    else WeakStream.empty[PullType]

  def sectionEvents(i : Int, max : Int) : WeakStream[PullType] = 
    sectionEvents(i, max, sectionEvents(i + 1,max))

  def withHeaders(max : Int) : WeakStream[PullType] = 
    events(sectionEvents(1 ,max))

  val Headers = List("root"l,"section"l,"sectionHeader"l)
  val OfInterest = List("root"l,"section"l,"ofInterest"l)

  val ofInterestOnDone = onDone(List(onQNames(Headers), onQNames(OfInterest)))


  def testFoldOnDone = {

    val ionDone = ofInterestOnDone

    /*
     * this test is more than a little intensive, given we are
     * purposefully quadratic so it explodes memory creation
     * as such we aren't linked to maxIterations.
     * 
     */
    val ourMax = 50

    val iter = withHeaders(ourMax).iterator

    val total = foldOnDone(iter)( (0, 0), ionDone ){ 
      (t, qnamesMatch) =>
	if (qnamesMatch.size == 0) {
	  t // no matches
	} else {
	  // only one at a time
	  assertEquals(1, qnamesMatch.size)
	  val head = qnamesMatch.head
	  assertTrue("Should have been defined",head._2.isDefined)
	  
	  // we should never have more than one child in the parent
	  // and thats us
	  assertEquals(1, head._2.get.zipUp.children.size)
/*	  val count = head._2.get.zipUp.children.size
	  if (count != 1) {
	    head._2.get.zipUp.children.foreach(println)
	    fail("Had more children "+ count)
	  }*/
	  val i =  text(head._2.get).toInt
	  if (head._1 eq Headers) {
	    assertEquals(t._1, t._2)
	    // get new section
	    (i, 1)
	  } else {
	    (t._1, i)
	  }
	}
    } 
    assertEquals(ourMax - 1, total._1)
    assertEquals(total._1, total._2)
    
  }

/*
  def testEphemeral = {
    def ntimes( i : Int, max : Int) : EphemeralStream[Int] =
      if (i < max) 
	EphemeralStream[Int](i) ++ (ntimes( i + 1, max))
      else
	EphemeralStream.empty

    val itr = ntimes(0, 300000).iterator
    println(itr.next)
  }*/

  type FiveStrings = (String,String,String,String,String)

  def testIterator = {
    val pull = pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = List("log"l,"logentry"l)

    var i = 0

    val it = scales.xml.iterate(LogEntries, pull.it)
    val bits = for{ entry : XmlPath <- it
	revision <- entry.\.*@("revision"l).one
	author <- entry.\*("author"l).one
	path <- entry.\*("paths"l).|>{x=> i+=1;x}.\*("path"l)
	kind <- path.\.*@("kind"l)
	action <- path.\.*@("action"l)
    } yield (text(revision), value(author), text(kind), text(action), value(path))

    val t = bits.next//iterator.next
    //println( t._1+ " " + t._2 + " " + t._3 + " " + t._4 + " " + t._5 )
    assertEquals(1, i)
    assertEquals(expectedHead, t)

    // sanity check
    if (it.isTraversableAgain) assertTrue(!it.isEmpty)

    pull.close
  }

  val expectedHead = ("264","chris.twiner","dir","M","/trunk/scalesUtils")

  def testIteratorCombo = {
    val pull = pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = List("log"l,"logentry"l)
    val ionDone = onDone(List(onQNames(LogEntries)))

    val entries = foldOnDone(pull.it)( List[FiveStrings](), ionDone ){ 
      (t, qnamesMatch) =>
	if (qnamesMatch.size == 0) {
	  t // no matches
	} else {
	  val entry = qnamesMatch.head._2.get
	  val bits = for{ 
			 revision <- entry.\.*@("revision"l).one
			 author <- entry.\*("author"l).one
			 path <- entry.\*("paths"l).\*("path"l)
			 kind <- path.\.*@("kind"l)
			 action <- path.\.*@("action"l)
		       } yield (text(revision), value(author), text(kind), text(action), value(path))
	  t ++ bits
	}
    }
    
    assertTrue("Pull was not closed",pull.isClosed)
    assertEquals(expectedHead, entries.head)
    assertEquals(("51","chris.twiner","dir","A","/trunk/scalesUtils"),entries.last)
    //entries.foreach(println)
  }

  def testSkipTop = { 
    val iter = events(10).iterator
       
    var res = skip(List())(iter) run

    assertEquals("{}root", qualifiedName(res.get))

    res = skip(List())(iter) run

    assertEquals("{}unloved", qualifiedName(res.get))
  }

  def testSkipSoap = {
    val iter = events(10).iterator
       
    var res = skip(List(2, 1))(iter) run

    val path = res.get
    assertEquals("{}interesting", qualifiedName(path))
    assertEquals(1, path.zipUp.children.size)
    assertEquals(1, path.zipUp.zipUp.children.size)

  }

  def testSkipTooFar = {
    val iter = events(2).iterator
       
    val res = skip(List(20, 1))(iter) run

    assertTrue("Should not have found anything", res.isEmpty)
  }

  def testSkipNoMatch = {
    def iter = events(2).iterator
       
    var res = skip(List(1, 20))(iter) run

    assertTrue("Should not have found anything", res.isEmpty)

    res = skipv(1, 1, 20)(iter) run

    assertTrue("Should not have found anything", res.isEmpty)
  }
}
