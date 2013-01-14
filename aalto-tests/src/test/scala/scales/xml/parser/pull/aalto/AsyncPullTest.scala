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

  import scales.utils.{resource => sresource}

  import DangerousIterateeImplicits._ // toRunEval
  import TestIteratees._

  val smallBufSize = 10
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = smallBufSize )

  import serializers._

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
//	  println("first is " + p)
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

    //println(" e is " + e)
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
      val enumeratee = enumToManyAsync(iter)(AsyncParser.parse(p))
      val (HasResult(e),cont) = enumeratee(wrapped).run
      e
    }

  // Just using the parser
  def testRandomAmountsDirectParser = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()
    
    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize)

    val parser = AsyncParser()

    val empty = () => EphemeralStream.empty

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
    
    //println("got a zero len "+randomChannel.zeroed+" times. Nexted "+nexted+" - headed "+headed)
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertEquals(randomChannel.zeroed + 1, nexted)
  }

  // using the parser and the parse iteratee
  def testRandomAmountsParse = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()

    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize)

    val parser = AsyncParser()

    val empty = () => EphemeralStream.empty

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
	  done = (a, y) => { 
	    val (e, cont) = a
	    headed += 1
	    //println("got here "+ headed)
	    var st = e
	    while(!st.isEmpty) {
	      val h = st.head()
	      res = res :+ h
	      st = st.tail()
	    }
	    cont.asInstanceOf[ResumableIter[DataChunk, EphemeralStream[PullType]]]
	  },
	  cont = k => {
	    nexted += 1
	    k(input)
	  }
	)

      c = nextC 

      if (isDone(c)) {
	b = randomChannel.nextChunk
	if (b == EOFData) {
	  //do it one last time
	  c = nextC
	}
      }
    }
    
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)
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

    val enumeratee = enumToManyAsync(iter)(AsyncParser.parse(parser))
    val wrapped = new ReadableByteChannelWrapper(randomChannel, true, tinyBuffers)
    val cstable = enumeratee(wrapped).eval
    var c = cstable
    
    type cType = cstable.type

    val FEEDME : Option[AsyncOption[(XmlOutput, Option[Throwable])]] = Some(NeedsMoreData)

//    var c = iter(parser).eval
    var count = 0
    while((extract(c) == FEEDME) || (!isDone(c))) {
      c = extractCont(c)(wrapped).eval
      count += 1
      //println("evals")
    }
//    println("eval'd "+ count +" times ") 
//    println("got a zero len "+ randomChannel.zeroed+" times ")

    assertEquals("eval "+count+" zero "+randomChannel.zeroed, count, randomChannel.zeroed)

//    println("is " + c.fold( done = (a, i) => i, cont = k => ""))
    
//    println("Got a "+extract(c))
//    println("was " + strout.toString)

    c.fold[Unit](
      done = (a,i) => {
	val (HasResult((out, thrown)), cont) = a
	assertFalse( "shouldn't have thrown", thrown.isDefined)
	//println(" iter was " + strout.toString)
	assertTrue("should have been auto closed", closer.isClosed)
	assertEquals(str, strout.toString)
      },
      cont = f => error("Should have been done")
    )
    
    assertTrue("Parser should have been closed ", parser.isClosed)
    assertTrue("Wrapper should have been closed ", wrapped.isClosed)

    // the output stream is closed, the input stream and parser is closed
    assertTrue("should have been EOF", isEOF(c))
  }

  def testSimpleLoadSerializingMisc = {
    val url = sresource(this, "/data/MiscTests.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
//    println("doc miscs p "+doc.prolog.misc+ " e "+ doc.end.misc)
    val str = asString(doc) // especially needed here as we may have whitespace which isn't collected.

//    println("asString is " + str)
    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val ((out, thrown), cont) = enumeratee(channel: ReadableByteChannelWrapper[DataChunk]).runEval

    // we can swallow the lot, but endmiscs don't know there is more until the main loop, which needs evaling
//    val (out, thrown) = iter(parser).runEval
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

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout , doc )

    val enumeratee = enumToMany(iter)(AsyncParser.parse(parser))
    val ((out, thrown), cont) = enumeratee(channel: ReadableByteChannelWrapper[DataChunk]).run

    // we can swallow the lot
//    val (out, thrown) = iter(parser).run
    assertFalse( "shouldn't have thrown", thrown.isDefined)
//    println(" iter was " + strout.toString)
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
    val (e, cont) = enumeratee(channel: ReadableByteChannelWrapper[DataChunk]).run

    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    assertTrue("The parser should have been closed", parser.isClosed)
  }
  
}
