package scales.xml.aalto

class AsyncPullTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._
  import java.nio.channels._

  import scales.utils._
  import ScalesUtils._
  import scales.xml._
  import ScalesXml._

  import Functions._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import scalaz._
  import Scalaz._
  import scalaz.IterV._

  import scales.utils.{resource => sresource}

/**
 * Runs a previously eval'd continuation to completion - no "Diverging Iteratee" on the first iteration from a cont but may give "Confused Iteratee" when not finding an end state.  Its not in the standard utils as its dangerous and requires knowledge of both the enumerator and the data it enumerates.
 */ 
trait RunEval[WHAT,RETURN] {
  
  val orig : IterV[WHAT, RETURN]

  def runEval : RETURN = {
    var i = orig
    while(!isDone(i)) {
      i = i.fold(done = (x, y) => Done(x,y),
	 cont = k => k(EOF[WHAT]))
    }
    i.fold(done = (x, _) => x,
	 cont = k => error("Confused Iteratee!"))
  }
}

//trait IterateeImplicits {
  implicit def toRunEval[WHAT, RETURN]( i : IterV[WHAT, RETURN] ) = new RunEval[WHAT, RETURN] {
    lazy val orig = i
  }

  
  val aenum = new AsyncParserEnumerator()

  implicit val enum : Enumerator[AsyncParser] = aenum

//  def reEval[E,A]( iter : 
 
  /**
   * Drain through all, returning the last
   */ 
  def evalAll[FROM,TO](init : TO, f : (FROM) => TO ) : IterV[FROM, TO] = {
    def step(last : TO)(s: Input[FROM]): IterV[FROM, TO] =
      s(el = e => {
	val to = f(e)
//	println(" about to cont to")
	Cont(step(to)) // swallow them all
      },
        empty = {
//	  println(" about to cont last")
	  Cont(step(last))
	},
        eof = {
//	  println(">>>>>>>> last is "+last)	  
	  Done(last, IterV.EOF[FROM])
	}
	)
    Cont(step(init) _)
  }

  val smallBufSize = 10
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = smallBufSize )

  import serializers._

  /**
   * Hideously spams with various sizes of data, and more than a few 0 lengths.
   *
   * The aim is to test the proverbial out of the asynch code.  It should be able to handle being called with a single byte repeatedly.
   */
  def testRandomAmounts = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()

    val ourbuf = Array.ofDim[Byte](smallBufSize)

    val rand = new scala.util.Random()

    var zerod = 0

    val randomChannel = new java.nio.channels.ReadableByteChannel {
      var closed = false
      def read( buf : java.nio.ByteBuffer ) : Int = {
	val red = {
	  val t = rand.nextInt(smallBufSize)
	  if (t == 0) {
	    zerod += 1
	    1
	  } else t
	}
	val did = stream.read(ourbuf, 0, red)
	if (did > -1) {
	  buf.put(ourbuf, 0, did)
	}
	did
      }
      def close = { closed = true; stream.close }
      def isOpen = !closed
    }

    val parser = AsyncParser(randomChannel, bytePool = tinyBuffers)

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    var c = iter(parser).eval
    var count = 1
    while(!isDone(c)) {
      count += 1
      c = c.eval
    }
    println("eval'd "+ count +" times ") 
    println("got a zero len "+zerod+" times ")

    assertTrue("should have been EOF", isEOF(c))

    c.fold[Unit](
      done = (a,i) => {
	val (out, thrown) = a
	assertFalse( "shouldn't have thrown", thrown.isDefined)
	//println(" iter was " + strout.toString)
	assertTrue("should have been auto closed", closer.isClosed)
	assertEquals(str, strout.toString)
      },
      cont = f => error("Should have been done")
    )

  }

//Array[Byte] => PullType  

  def testSimpleLoadSerializingMisc = {
    val url = sresource(this, "/data/MiscTests.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
//    println("doc miscs p "+doc.prolog.misc+ " e "+ doc.end.misc)
    val str = asString(doc) // especially needed here as we may have whitespace which isn't collected.

//    println("asString is " + str)
    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    // we can swallow the lot, but endmiscs don't know there is more until the main loop, which needs evaling
    val (out, thrown) = iter(parser).runEval
    assertFalse( "shouldn't have thrown", thrown.isDefined)
//    println(" iter was " + strout.toString)
    assertTrue("should have been auto closed", closer.isClosed)
    assertEquals(str, strout.toString)
  }

  def testSimpleLoadSerializing = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

//    println("asString is " + str)
    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    // we can swallow the lot
    val (out, thrown) = iter(parser).run
    assertFalse( "shouldn't have thrown", thrown.isDefined)
//    println(" iter was " + strout.toString)
    assertTrue("should have been auto closed", closer.isClosed)
    assertEquals(str, strout.toString)
  }

  def testSimpleLoadTinyBufferRunEval = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel, bytePool = tinyBuffers)

    val iter = evalAll(Left(Text("I is a fake")), (p : PullType) => {
//      println(p)
      p} )

    var c = iter(parser).eval
    assertFalse(isDone(c))
    c = c.eval
    assertFalse(isDone(c))
    c = c.eval
    assertFalse(isDone(c))
    c = c.eval
    assertFalse(isDone(c))
    //c = c.eval
    
    // finalle

    val e = c.runEval
    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    // purposefully small, well we are async here
  }

  /**
   * ensure that the enumerator doesn't break basic assumptions when it can get
   * all the data
   */ 
  def testFlatMapMultipleDones = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel) // let the whole thing be swallowed in one
    
    val iter = 
      for {
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	i <- evalWith((p : PullType) => {
//	  println("first is " + p)
	  p} )
	j <- dropWhile((p : PullType) => {
	   p.fold( x => !x.isInstanceOf[Elem] , y => false)
	  } )
      } yield j
    
    val e = iter(parser).run
    assertTrue("Should be defined", e.isDefined)
    assertEquals("{urn:default}DontRedeclare", e.get.left.get.asInstanceOf[Elem].name.qualifiedName)
    parser.closeResource
  }

/*  can't work with normal run as we must bring back a cont, there is no way of moving forward without data. */
  def testSimpleLoadTinyBufferRunEvalSeperateState = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel, bytePool = tinyBuffers)

    val iter = evalAll(Left(Text("I is a fake")), (p : PullType) => {
//      println(p)
      p} )


    var c = iter(parser).eval
    assertFalse(isDone(c))
    //c = c.eval
    while(!parser.startedElementProcessing) {
      c = c.eval
    }
    
    assertFalse("Should not have been done, much more to process", isDone(c))

    // as it has now started and the
    // parser maintains state, new iter should be working on that
    
    val e = iter(parser).runEval
    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    // purposefully small, well we are async here
  }

/* */
  def testSimpleLoad = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val iter = evalAll(Left(Text("")), (p : PullType) => {
      //println(p); 
      p} )

    val e = iter(parser).run
    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
  }

  /**
   * Enumeratee that converts input 1:1
   * String => Int, enumerator Iterator[String] but IterV[Int, Int]
   */
  def enumerateeMap[E, A, R]( dest : IterV[A,R])( f : E => A ) : IterV[E, R] = {
    
    def next( i : IterV[A,R] ) : IterV[E, R] =
      i.fold( 
	done = (a, y) => Done(a, IterV.EOF[E]),
	cont = k => Cont((x: Input[E]) => 
	  x( el = e => next(k(IterV.El(f(e)))),
	    empty = next(k(IterV.Empty[A])),
	     eof = next(k(IterV.EOF[A])))
//	  next(k(x))
	)
      )

    next(dest)
  }

  def sum[T](implicit n: Numeric[T]): IterV[T,T] = {
    import n._
    def step(acc: T)( s : Input[T] ) : IterV[T, T] =
      s( el = e => Cont(step(acc + e)),
	empty = Cont(step(acc)),
	eof = Done(acc, IterV.EOF[T])
      )
    Cont(step(zero))
  }

  def testSimpleEnumerateeMap = {
    
    val i = List("1","2","3","4").iterator

    val res = enumerateeMap(sum[Int])( (s : String) => s.toInt )(i).run
    assertEquals(10, res)
  }
  
  /**
   * Takes a function f that turns input into Seq of a different type.
   * This function must return an ResumableIter in order to capture early
   * Done's without losing intermediate chunks.  If the dest iteratee is Done
   * any remaining continuation will also restart with dest.
   */ 
  def enumerateeOneToMany[E, A, R]( dest: IterV[A,R])( f: E => Seq[A]): ResumableIter[E, R] = {
    val empty = Seq()
 /*   def loop( i: IterV[A,R], s: Seq[A] ): ResumableIter[E, R] =
      if (!s.isEmpty)
	i.fold(
	  done = (a, y) => next( i, s ),// send it back
	  cont = 
	    k => loop(k(IterV.El(s.head)), s.tail)
	    )
      else next(i, s) // send it back
*/
    def loop( i: IterV[A,R], s: Seq[A] ): ResumableIter[E, R] = {
      var c: IterV[A,R] = i
      var cs: Seq[A] = s

      while(!isDone(c) && !cs.isEmpty) {
	val (nc, ncs): (IterV[A,R], Seq[A]) = c.fold(
	  done = (a, y) => (cs, s),// send it back
	  cont = 
	    k => (k(IterV.El(cs.head)), cs.tail)
	    )
	c = nc
	cs = ncs
      }
      next(c, cs) // let it deal with it.
    }

    def next( i: IterV[A,R], s: Seq[A] ): ResumableIter[E, R] =
      i.fold(
	done = (a, y) => Done((a, 
			       if (s.isEmpty)
				 Done(a, IterV.EOF[E])
			       else 
				 next(dest, s)
			     ), IterV.EOF[E]),
	cont = 
	  k => {
	    if (!s.isEmpty) 
	      loop(i, s)
	    else
	      Cont((x: Input[E]) => 
		x( el = e => {
		  val news = f(e)
		  next(k(IterV.El(news.head)), news.tail)
		},
		  empty = next(k(IterV.Empty[A]), empty),
		  eof = next(k(IterV.EOF[A]), empty)
		))
	  }
      )

    next(dest, empty)
  }
  

  def testEnumerateeOneToManyExhaust = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = enumerateeOneToMany(sum[Int])( (i: Int) => (1 to i).toSeq )(i).run
    assertEquals(20, res)
    assertTrue("should have been done", isDone(cont))
  }

  /**
   * Make sure it doesn't soe in the loop
   */ 
  def testEnumerateeOneToManySOE = {
    val i = List[Long](1,2,3,4,5).iterator
    
    val (res, cont) = enumerateeOneToMany(sum[Long])( (_:Long) => (1L to 100000L).toSeq )(i).run
    assertEquals(25000250000L, res)
    assertTrue("should have been done", isDone(cont))
  }

  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // bad, but simpler to test against before a resumableiter version
    var gotto = 0L

    val sum: IterV[Long,Long] = {
      def initial = (s : Input[Long]) =>
	step(gotto)(s) 
      
      def step(acc: Long)( s : Input[Long] ) : IterV[Long, Long] =
	s( el = e => 
	    if ((acc + e) == 25000050001L) {
	      gotto = acc + e
	      Done(gotto, IterV.EOF[Long])
	    } else
	      Cont(step(acc + e)),
	  empty = Cont(step(acc)),
	  eof = Done(acc, IterV.EOF[Long])
	)

      Cont(initial)
    }
    
    val (res, cont) = enumerateeOneToMany(sum)( (_:Long) => (1L to 100000L).toSeq )(i).run
    assertEquals(25000050001L, res)
    assertTrue("should have been done", !isDone(cont))

    val (res2, cont2) : ((Long,ResumableIter[Long, Long])) = cont.run
    assertEquals(25000250000L, res2)
    assertTrue("should have been done", isDone(cont2))
  }

  /**
   * Enumeratee that folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   *
   * Converts ResumableIters on Done via a fold, returning Done only when receiving EOF from the initResumable.
   *
   * NB - This can be thought of the reverse of toResumableIter but also accumulating.
   */
  def foldOnDoneIter[E,A, ACC]( initAcc : ACC, initIter : ResumableIter[E,A])( f : (ACC, A) => ACC ) : IterV[E, ACC] = {
    
    def next( acc : ACC, i : ResumableIter[E,A]) : IterV[E, ACC] = 
      i.fold( 
	done = (ac, y) => {
	  val (e, cont) = ac 
	  val newacc = f(acc, e)
	  y match {
	    case IterV.El(a) =>
	      error("Cannot process an input element from Done")
	    case IterV.Empty() => 
	      // can continue
	      Cont( (x : Input[E]) => 
		// is the cont itself actually a don or a cont?
		next(newacc, 
		     cont match {
		       case Done(x,y) => error("got a Done from a resumableIter cont "+ x +" "+y)
		       case Cont(k) => 
			 k(x).asInstanceOf[ResumableIter[E,A]]
		     }
		      ))
	    case IterV.EOF() => Done(newacc, IterV.EOF[E])
	  }
	},
	cont = k => Cont((x: Input[E]) => 
	  next(acc, k(x)))
      )

    next(initAcc, initIter)
  }

  def testSimpleLoadAndFold = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val ns = Namespace("urn:default")

    val iter = foldOnDoneIter( List[String](), 
      onQNames(List(ns("Default"), "NoNamespace"l,"DontRedeclare"l))){
	(l, qmatch) => qmatch._2.  // its empty as it was eof
	  map(p => qname(p.tree) :: l).getOrElse( l )
      }

    val e = iter(parser).run

    //println(" e is " + e)
    assertEquals(2, e.size)
    e match {
      case List("DontRedeclare", "DontRedeclare") => ()
      case _ => fail("got "+e)
    }
  }
  

}
