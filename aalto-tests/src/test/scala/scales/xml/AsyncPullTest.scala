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
 * Runs a previously eval'd continuation to completion - no "Diverging Iteratee" on the first iteration from a cont but may give "Confused Iteratee" when not finding an end state.
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

  import java.nio.charset.Charset

  type SerialIterT = IterV[PullType, (XmlOutput, Option[Throwable])] 

  /**
   * The serializer will be returned automatically to the pool by calling closer
   * 
   * doc functions are only evaluated upon the first elem / last elem
   */
  def serializeIter( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc()) : SerialIterT = {

    var empties = 0

    def done( status : StreamStatus ) : SerialIterT = {
      // give it back
      closer()
      println("empties was "+empties)
      Done((status.output, status.thrown), EOF[PullType])
    }

    def rest( status : StreamStatus, prev : PullType, serializer : Serializer )(s : Input[PullType]) : SerialIterT = {
      s(el = e => {
	if (status.thrown.isDefined) done(status)
	else {
	  val r = StreamSerializer.pump((prev, e), status, serializer)
	  if (r.thrown.isDefined) done(r)
	  else Cont(rest(r, e, serializer))
	}
	},
        empty = {
	  empties += 1
	  Cont(rest(status, prev, serializer))
	},
        eof =  {
	if (status.thrown.isDefined) done(status)
	else {
	  val r = StreamSerializer.pump((prev, StreamSerializer.dummy), status, serializer)
	  // TODO add end misc
	  done(r)
	}})
    }
    
    def first( status : StreamStatus, serializer : Serializer )(s : Input[PullType]) : SerialIterT =
      s(el = e => {
	// decl and prolog misc, which should have been collected by now
	val opt = serializer.xmlDeclaration(status.output.data.encoding, 
				  status.output.data.version).orElse{
	    serializeMisc(status.output, doc.prolog.misc, serializer)._2
	  }
	val nstatus = status.copy(thrown = opt)
	  
	Cont(rest(nstatus, e, serializer))
	},
        empty = {
	  empties += 1
	  Cont(first(status, serializer))
	},
        eof = Done((status.output, Some(NoDataInStream())), EOF[PullType]))

    Cont(first(StreamStatus(output), serializer))
  }

  /**
   * Returns an Iteratee that can serialize PullTypes to out.  The serializer factory management is automatically handled upon calling with eof.  This can be triggered earlier by calling closeResource on the returned CloseOnNeed.
   */ 
  def pushXml( out: java.io.Writer, doc : DocLike = EmptyDoc(), version: Option[XmlVersion] = None, encoding: Option[Charset] = None )(implicit serializerFI: SerializerFactory) : (CloseOnNeed, SerialIterT) = {

    val decl = doc.prolog.decl
    val sd = SerializerData(out, 
      version.getOrElse(decl.version), 
      encoding.getOrElse(decl.encoding))

    val xo = XmlOutput(sd)(serializerFI)

    val ser = serializerFI.borrow( sd ) 

    val closer : CloseOnNeed = new CloseOnNeed {
      def doClose() {
	serializerFI.giveBack(ser)
      }
    }
    val iter = serializeIter( xo, ser, () => closer.closeResource, doc)

    (closer, iter)
  }

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
    val (closer, iter) = pushXml( strout , doc )

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

  // TODO end misc

  def testSimpleLoadSerializing = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

//    println("asString is " + str)
    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXml( strout , doc )

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

  

}
