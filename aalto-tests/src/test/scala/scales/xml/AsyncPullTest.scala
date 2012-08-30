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
  
  val aenum = new AsyncParserEnumerator()

  implicit val enum : Enumerator[AsyncParser] = aenum
 
  /**
   * Drain through all, returning the last
   */ 
  def evalAll[FROM,TO](init : TO, f : (FROM) => TO ) : IterV[FROM, TO] = {
    def step(last : TO)(s: Input[FROM]): IterV[FROM, TO] =
      s(el = e => {
	val to = f(e)
	Cont(step(to)) // swallow them all
      },
        empty = Cont(step(last)),
        eof = {
//	  println(">>>>>>>> last is "+last)
	  Done(last, IterV.EOF[FROM])
	}
	)
    Cont(step(init) _)
  }
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = 10 )

  import serializers._

  import java.nio.charset.Charset

  type SerialIterT = IterV[PullType, (XmlOutput, Option[Throwable])] 

  /**
   * The serializer will be returned automatically to the pool by calling closer
   * 
   * doc functions are only evaluated upon the first elem / last elem
   */
  def serializeIter( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc()) : SerialIterT = {
    def done( status : StreamStatus ) : SerialIterT = {
      // give it back
      closer()
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
        empty = Cont(rest(status, prev, serializer)),
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
        empty = Cont(first(status, serializer)),
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

  def testSimpleLoadSerializing = {
    aenum.reset
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val str = asString(loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool).rootElem)

//    println("asString is " + str)
    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel)

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXml( strout )

    // we can swallow the lot
    val (out, thrown) = iter(parser).run
    assertFalse( "shouldn't have thrown", thrown.isDefined)
//    println(" iter was " + strout.toString)
    assertTrue("should have been auto closed", closer.isClosed)
    assertEquals(str, strout.toString)
  }

  def testSimpleLoadTinyBuffer = {
    aenum.reset
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser(channel, bytePool = tinyBuffers)

    val iter = evalAll(Left(Text("I is a fake")), (p : PullType) => {
//      println(p)
      p} )

    var c = iter(parser).eval
    assertFalse(isDone(c))
    c = iter(parser).eval
    assertFalse(isDone(c))
    c = iter(parser).eval
    assertFalse(isDone(c))
    c = iter(parser).eval
    
    assertTrue("Should have pumped a whole lot of doc now" + c, isDone(c))
    
    // finalle

    val e = c(parser).run
    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    // purposefully small, well we are async here
  }

/* */
  def testSimpleLoad = {
    aenum.reset
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
