package scales.xml.parser.pull.aalto

class AsyncPullTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._
  import java.nio.channels._

  import scales.utils._
  import ScalesUtils._
  import scales.xml._
  import ScalesXml._
 
  import scales.xml.impl.NoVersionXmlReaderFactoryPool

  import io._
  import ScalesUtilsIO._

  import Functions._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import scalaz._
  import Scalaz._
  import scalaz.IterV._
  import EphemeralStream.emptyEphemeralStream

  import scales.utils.{resource => sresource}

  import DangerousIterateeImplicits._ // toRunEval
  import TestIteratees._

  val smallBufSize = 10
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = smallBufSize )

  import serializers._


  type SerialIterT = IterV[PullType, (XmlOutput, Option[Throwable])] 

  import java.nio.charset.Charset
  import scales.utils.{io, resources}
  import resources._ 

  /**
   * returns the cont and drops the input parameter or returns Done
   */ 
  trait EvalW[WHAT,RETURN] {
    
    val orig : IterV[WHAT, RETURN]

    def evalw : IterV[WHAT, RETURN] = {
      orig.fold(done = (x, y) => Done(x,y),
		cont = k => {
		  orig
		})
    }
  }

  implicit def toEvalw[WHAT, RETURN]( i : IterV[WHAT, RETURN] ) = new EvalW[WHAT, RETURN] {
    lazy val orig = i
  }

  /**
   * ensure that the enumerator doesn't break basic assumptions when it can get
   * all the data
   */
  def testFlatMapMultipleDones = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser() // let the whole thing be swallowed in one
    
    val iter = 
      for {
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	_ <- peek[PullType]
	i <- evalWith((p : PullType) => {
	  p} )
	j <- dropWhile((p : PullType) => {
	   p.fold( x => !x.isInstanceOf[Elem] , y => false)
	  } )
      } yield j
    
    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val wrapped = new ReadableByteChannelWrapper(channel, true, tinyBuffers)
    
    val (e,cont) = enumeratee(wrapped).run
    assertTrue("Should be defined", e.isDefined)
    assertEquals("{urn:default}DontRedeclare", e.get.left.get.asInstanceOf[Elem].name.qualifiedName)
    parser.closeResource
    wrapped.closeResource
  }

  def testSimpleLoadAndFold =
    doSimpleLoadAndFold{
      (p, iter, wrapped) => 
      val enumeratee = enumToMany(iter)(AsyncParser.parse(p))
      val (e,cont) = enumeratee(wrapped).run
      e
    }

  def doSimpleLoadAndFold[T](test: (AsyncParser, IterV[PullType, List[String]], ReadableByteChannelWrapper[DataChunk ]) =>  List[String] ) : Unit = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser()

    val ns = Namespace("urn:default")

    val iter = foldOnDoneIter( List[String](), 
      onQNames(List(ns("Default"), "NoNamespace"l,"DontRedeclare"l))){
	(l, qmatch) => qmatch._2.  // its empty as it was eof
	  map(p => qname(p.tree) :: l).getOrElse( l )
      }

    val wrapped = new ReadableByteChannelWrapper(channel)
    val e = test(parser, iter, wrapped)

    assertEquals(2, e.size)
    e match {
      case List("DontRedeclare", "DontRedeclare") => ()
      case _ => fail("got "+e)
    }
  }

  def testSimpleLoadAndFoldAsync =
    // should have collected all anyway
    doSimpleLoadAndFold{
      (p, iter, wrapped) => 
      val enumeratee = enumToMany(iter)(AsyncParser.parse(p))
      val (e,cont) = enumeratee(wrapped).run
      e
    }
// here

  // Just using the parser
  def testRandomAmountsDirectParser = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()
    
    val randomChannelOrig = new RandomChannelStreamWrapper(stream, smallBufSize)
    val randomChannel = randomChannelOrig.wrapped    

    val parser = AsyncParser()

    val empty = () => emptyEphemeralStream

    var nexted = 0
    var headed = 0

    var res = Vector.empty[PullType]
    
    var b : DataChunk = EmptyData
    while(b != EOFData) {
      b = randomChannel.nextChunk
      val s = parser.nextInput(b)
      s(
	el = e => {
	  headed += 1
	  var st = e
	  while(!st.isEmpty) {
	    val h = st.head()
	    res = res :+ h
	    st = st.tail()
	  }
	},
	empty = {nexted += 1;()},
	eof = {nexted += 1;()}
      )
    }
    
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertTrue("we should have more nexted then zeroed - due to boundaries on the available data", randomChannelOrig.zeroed + 1 < nexted)
  }

  // using the parser and the parse iteratee
  def testRandomAmountsParse = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()

    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize).wrapped

    val parser = AsyncParser()

    val empty = () => emptyEphemeralStream

    var nexted = 0
    var headed = 0

    var res = Vector.empty[PullType]

    var c = AsyncParser.parse(parser)
    
    var b : DataChunk = EmptyData
    while(b != EOFData) {
      def input =
	if (b.isEOF)
	  IterV.EOF[DataChunk]
	else
	  if (b.isEmpty) {
	    IterV.Empty[DataChunk]
	  } else
	    El(b)
      
      def nextC = 
	c.fold (
	  done = (a, y) => { // if eof
	    val (e, cont) = a
	    headed += 1

	    var st = e
	    while(!st.isEmpty) {
	      val h = st.head()
	      res = res :+ h
	      st = st.tail()
	    }
	    // is
	    cont.asInstanceOf[ResumableIter[DataChunk, EphemeralStream[PullType]]]
	  },
	  cont = k => {
	    nexted += 1
	    // need to push the next chunk
	    k(input)
	  }
	)

      // if its done we have to pump
      if (!randomChannel.isClosed && !isDone(c))
	b = randomChannel.nextChunk

      c = nextC
    }
    
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertTrue("Cont should have been eof", isEOF(c))
    assertTrue("Parser should have been closed", parser.isClosed)
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

    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize)
    
    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val wrapped = new ReadableByteChannelWrapper(randomChannel, true, tinyBuffers)
    
    /*
     * the random channel does every 10, every 6 forces it back but allows
     * a fair chunk of Empty -> Conts followed by data
     */ 
    implicit val readableByteChannelEnumerator = new AsyncDataChunkerEnumerator( 6 )

    val cstable = enumeratee(wrapped).evalw
    var c = cstable
    
    type cType = cstable.type

    var count = 0
    while(!isDone(c)) {
      c = c(wrapped).evalw
      count += 1
    }

    if (randomChannel.zeroed > 0) {
      assertTrue("There were "+randomChannel.zeroed+" zeros fed but it never left the evalw", count > 0)
    }

    c.fold[Unit](
      done = (a,i) => {
	val ((out, thrown), cont) = a
	assertFalse( "shouldn't have thrown", thrown.isDefined)

	assertTrue("should have been auto closed", closer.isClosed)
	assertEquals(str, strout.toString)
      },
      cont = f => fail("Should have been done")
    )
    
    assertTrue("Parser should have been closed ", parser.isClosed)
    assertTrue("Wrapper should have been closed ", wrapped.isClosed)

    // the output stream is closed, the input stream and parser is closed
    assertTrue("should have been EOF", isEOF(c))
  }

  def testSimpleLoadSerializingMisc = {
    val url = sresource(this, "/data/MiscTests.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc) // especially needed here as we may have whitespace which isn't collected.

    val channel = Channels.newChannel(url.openStream()).wrapped

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val ((out, thrown), cont) = enumeratee(channel).runEval

    // we can swallow the lot, but endmiscs don't know there is more until the main loop, which needs evaling
    assertFalse( "shouldn't have thrown", thrown.isDefined)

    assertTrue("should have been auto closed", closer.isClosed)
    assertEquals(str, strout.toString)
  }

  def testSimpleLoad = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val iter = evalAll(Left(Text("")), (p : PullType) => {
//      println(p); 
      p} )

    val parser = AsyncParser()
    
    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val (e, cont) = enumeratee(channel.wrapped).run

    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    assertTrue("The parser should have been closed", parser.isClosed)
  }

  def testSimpleLoadSerializing =
    doSimpleLoadSerializing(Channels.newChannel(_).wrapped)

  def doSimpleLoadSerializing(streamToChannel: java.io.InputStream => DataChunker[DataChunk]) = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val channel = streamToChannel(url.openStream())

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    val enumeratee = enumToMany(iter)(parser.iteratee)
    val ((out, thrown), cont) = enumeratee(channel).run

    // we can swallow the lot
    assertFalse( "shouldn't have thrown", thrown.isDefined)

    assertTrue("should have been auto closed", closer.isClosed)
    assertEquals(str, strout.toString)

    assertTrue("Channel itself should have been auto closed", channel.isClosed)
  }

  def testSimpleLoadSerializingDirect =
    doSimpleLoadSerializing( s => new ReadableByteChannelWrapper(Channels.newChannel(s), bytePool = new DirectBufferPool()))

  def testSimpleLoadSerializingDirectSmallArrays = 
    doSimpleLoadSerializing{ s => 
      new ReadableByteChannelWrapper(Channels.newChannel(s), bytePool = new DirectBufferPool(), directBufferArrayPool = new ByteArrayPool(50))
    }

  def testSimpleLoadSerializingSmallDirectLargerArrays = 
    doSimpleLoadSerializing{ s => 
      new ReadableByteChannelWrapper(Channels.newChannel(s), bytePool = new DirectBufferPool(50)) 
    }

  def testSimpleLoadSerializingSmallDirectSmallArrays = 
    doSimpleLoadSerializing{ s => 
      new ReadableByteChannelWrapper(Channels.newChannel(s), bytePool = new DirectBufferPool(50), directBufferArrayPool = new ByteArrayPool(50))
    }

}
