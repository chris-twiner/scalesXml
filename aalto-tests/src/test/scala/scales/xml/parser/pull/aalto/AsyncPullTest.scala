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
   * The serializer will be returned automatically to the pool by calling closer
   * 
   * doc functions are only evaluated upon the first elem / last elem
   */ 
  def serializeIter( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc()) : SerialIterT = {

    var empties = 0

    def done( status : StreamStatus ) : SerialIterT = {
      // give it back
      closer()
      //println("empties was !!! "+empties)
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
	  //println("outitr empty")
	  Cont(rest(status, prev, serializer))
	},
        eof =  {
	if (status.thrown.isDefined) done(status)
	else {
	  val r = StreamSerializer.pump((prev, StreamSerializer.dummy), status, serializer)
	  val opt = serializeMisc(r.output, doc.end.misc, serializer)._2
	    
	  val lastStatus = r.copy(thrown = opt)
	  
	  done(lastStatus)
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
        eof = {
	  Done((status.output, Some(NoDataInStream())), EOF[PullType])
	})

    Cont(first(StreamStatus(output), serializer))
  }

  /**
   * Returns an Iteratee that can serialize PullTypes to out.  The serializer factory management is automatically handled upon calling with eof.  This can be triggered earlier by calling closeResource on the returned CloseOnNeed.
   */ 
  def pushXmlIter( out: java.io.Writer, doc : DocLike = EmptyDoc(), version: Option[XmlVersion] = None, encoding: Option[Charset] = None )(implicit serializerFI: SerializerFactory) : (CloseOnNeed, SerialIterT) = {

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
   * Defaults to continuing when Empty is returned by toMany for an Empty input.
   *
  def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R] = 
    enumToManyAsyncOption[E, A, R, R](
      _.getOrElse(error("No Asynchnronous Behaviour Expected But the toMany still recieved an Empty and returned a Done Empty")), // throw if async somehow got returned
      false // use cont instead
      )(dest)(toMany)
  */

  /**
   * Takes a function f that turns input into an Input[EphemeralStream] of a different type A.  The function f may return El(EphemeralStream.empty) which is treated as Empty.
   * This function must return an ResumableIter in order to capture early Done's without losing intermediate chunks,
   * the destination iter having the same requirements.
   *
   * The AsyncOption is required in the return to handle the case of empty -> empty infinite loops.  For asynchronous parsing, for example, we should be able to return an empty result but with Empty as the input type.
  

def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R] = {
    val empty = () => EphemeralStream.empty

    def loop( i: ResumableIter[A,R], s: () => EphemeralStream[A] ):
      (ResumableIter[A,R], () => EphemeralStream[A]) = {
      var c: ResumableIter[A,R] = i
      var cs: EphemeralStream[A] = s() // need it now
//println("loopy")
      while(!isDone(c) && !cs.isEmpty) {
	println("doing a loop "+c)
	val (nc, ncs): (ResumableIter[A,R], EphemeralStream[A]) = c.fold(
	  done = (a, y) => (c, s()),// send it back, shouldn't be possible to get here anyway due to while test
	  cont = 
	    k => {
	      println("got cont")
	      val head = cs.head() // if used in El it captures the byname not the value
	      (k(IterV.El(head)), cs.tail())
	    }
	    )
	c = nc
	cs = ncs
      }
      (c, () => cs)
    }

//    class BounceIt() extends 

    def contk( k: scalaz.Input[A] => ResumableIter[A,R],  i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, R] = {
      	    if (!s().isEmpty) {
	      val (ni, ns) = loop(i, s)
	      println("empty against the s "+ni + " " + ns)
	      //if (isDone(ni)) 
	      next(ni, ns, toMany) // bad - should let a done exit early
	    } else
	      Cont((x: Input[E]) => 
		x( el = e => {
		  println("got a cont x el e "+e)
		  toMany.fold (
		    done = (a, y) => {
		      val (e1, nextContR) = a
		      val nextCont = nextContR.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
		      error("Unexpected State for enumToMany - Cont but toMany is done")		     	
		    },
		    cont = y => {
/**/		      println("and then I was here "+
			      x(el = e => e.toString, 
				   empty = "I'm empty ",
				   eof = "I'm eof"))

		      val afterNewCall = y(x)
		      println("and then " + afterNewCall)

/*
 * So the first chunk is the only chunk, we have the first cont - should be done onthe cont?
 */ 

		      afterNewCall.fold(
			done = (nextContPair, rest) => {
			  println("was done wern't it")
			  val (e1, nextCont) = nextContPair
			  val nextContR = nextCont.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
			  if (isEOF(afterNewCall)) {
			    println("after is eof")
			    next(k(IterV.EOF[A]), empty, nextContR)
			  } else {
			    if (e1.isEmpty) {
			      println("empty on nextcontr")
			      next(k(IterV.Empty[A]), empty, nextContR)
			    }
			    else {
			      println("some data after all")
			      next(k(IterV.El(e1.head())), e1.tail, nextContR)
			    }
			  }
			},
			cont = k1 => {
			  println("conted after here")
			  next(k(IterV.Empty[A]), empty, afterNewCall)
			}
			)
		    }
		  )
		},
		  empty = {
		    next(k(IterV.Empty[A]), empty, toMany)
		  },
		  eof = {
		    next(k(IterV.EOF[A]), empty, toMany)
		  }
		))

    }

      
//    @scala.annotation.tailrec 
    def next( i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, R] =
      
	i.fold(
	done = (a, y) => {
//	  println(" y is "+y) 

	  val (res, nextCont) = a
	  println("res is "+ res) 

	  val returnThis : ResumableIter[E, R] = 
	  if ((isDone(nextCont) && isEOF(nextCont)) ||
	      (isDone(toMany) && isEOF(toMany))     || // either eof then its not restartable
	      (EOF.unapply(y)) // or the source is out of elements
	      ) { 
	    Done((res, Done(res, IterV.EOF[E])), IterV.EOF[E])
	  } else {
	    // there is a value to pass back out
println("iz here")
	    Done((res, 
	      {
	      Cont((x: Input[E]) => 
		x( el = e => {

		  // we don't actually want to call next but we can't defer it
		  // as we have to consume the current data - no Cont without an
		  // input === no way to have 1->X transformations and stop for every transformation.  the cont would have to cache inputs <-- not very smart for a constant space op.

		val n = next(nextCont.asInstanceOf[ResumableIter[A,R]], s, toMany)
		
		n
/*		if (s().isEmpty) {
		  println("got here with s().empty")
//		  new RuntimeException("Da fuck?!!").printStackTrace()

		  // there is no more data saved up
		  Done((res, n), IterV.Empty[E])
		} else {
		  println("non - empty")
		  // still data to process
		  n
		}*/
	      }), IterV.Empty[E])
	  }

	  if (EOF.unapply(y)) {
	    // signal the end here to toMany, don't care about result
	    toMany.fold(done= (a1, y1) => false,
			cont = k => {
			  k(IterV.EOF[E]); false
			})
	  }
	  
	  returnThis
	  },
	cont = 
	  k => {
	    contk(k, i, s, toMany)
//	    println("Fucksake")
	  }
      )

    next(dest, empty, toMany)
  }

     */ 


  implicit val readableByteChannelEnumerator: Enumerator[ReadableByteChannelWrapper] = asyncReadableByteChannelEnumerator( )

  /**
   * Use in a call to asyncReadableByteChannelEnumerator to turn it into a synchronous enumerator (constantly trying to get new chunks of data)
   */
  val INFINITE_RETRIES = -1

  /**
   * Creates an Enumerator with a given count for Empty -> Cont applications.
   *
   * When the count is met it returns the Cont for the next Enumeration step.
   *
   * Note: Call via eval only.
   * @param contOnCont INFINITE_RETRIES (-1) for keep on trying, the default is 5 (as exposed by the implicit enumerator readableByteChannelEnumerator)
   */
  def asyncReadableByteChannelEnumerator( contOnCont: Int = 5 ): Enumerator[ReadableByteChannelWrapper] = new Enumerator[ReadableByteChannelWrapper] {
    def apply[E,A](wrapped: ReadableByteChannelWrapper[E], i: IterV[E,A]): IterV[E, A] = {
 
      def apply(wrapped: ReadableByteChannelWrapper[E], i: IterV[E,A], count: Int): IterV[E, A] = {
	i match {
	  case _ if !wrapped.channel.isOpen || wrapped.isClosed => i
	  case Done(acc, input) => i
	  case Cont(k) =>
	    val realChunk = wrapped.nextChunk
	    val nextChunk = realChunk.asInstanceOf[E]
	    val nextI = 
	      if (realChunk.isEOF) {
		 // println("actual data was EOF !!!")
		  k(IterV.EOF[E])
		} else
		  if (realChunk.isEmpty)
		    k(IterV.Empty[E])
		  else
		    k(El(nextChunk))
	    val nc = 
	      if (realChunk.isEmpty && !isDone(nextI)) {
		count + 1
	      } else 0

	    if ((contOnCont != INFINITE_RETRIES) && (nc > contOnCont)) {
	      //println("had cont on cont count, returning")
	      nextI
	    } else
	      apply(wrapped, nextI, nc)
	}
      }

      apply(wrapped, i, 0)
    }
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
    
//    println("got a zero len "+randomChannel.zeroed+" times. Nexted "+nexted+" - headed "+headed)
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertTrue("we should have more nexted then zeroed - due to boundaries on the available data", randomChannel.zeroed + 1 < nexted)
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
	  done = (a, y) => { // if eof
	    val (e, cont) = a
	    headed += 1
	    //println("got here "+ headed)
	    var st = e
	    while(!st.isEmpty) {
	      val h = st.head()
	      res = res :+ h
	      st = st.tail()
	    }
	    // is
	    //println(" -> "+res)
	    cont.asInstanceOf[ResumableIter[DataChunk, EphemeralStream[PullType]]]
	  },
	  cont = k => {
	    nexted += 1
	    //println( "nexted " + nexted )
	    // need to push the next chunk
	    k(input)
	  }
	)

      // if its done we have to pump
      if (!randomChannel.isClosed && !isDone(c))
	b = randomChannel.nextChunk

      //println("next chunked " + b)
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
    implicit val readableByteChannelEnumerator = asyncReadableByteChannelEnumerator( 6 )

    val cstable = enumeratee(wrapped).evalw
    var c = cstable
//    println("eval - already donE?? " + isDone(c))
    
    type cType = cstable.type

    var count = 0
    while(!isDone(c)) {
      c = c(wrapped).evalw//extractCont(c)(wrapped).evalw
      count += 1
    }
//    println("evalw'd "+ count +" times ") 
//    println("got a zero len "+ randomChannel.zeroed+" times ")

    if (randomChannel.zeroed > 0) {
      assertTrue("There were "+randomChannel.zeroed+" zeros fed but it never left the evalw", count > 0)
    }

    c.fold[Unit](
      done = (a,i) => {
	val ((out, thrown), cont) = a
	assertFalse( "shouldn't have thrown", thrown.isDefined)
	//println(" iter was " + strout.toString)
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

/*  def testSimpleEnumerateeMap = {
    
    val i = List("1","2","3","4").iterator

    val res = enumerateeMap(sum[Int])( (s : String) => s.toInt )(i).run
    assertEquals(10, res)
  }
  
  def testEnumToManyExhaustState = {
    
    val l = List(1,2,3,4)

    def f(i: Int): ResumableIter[Int, EphemeralStream[Int]] = {
      def step(i: Int)(s: Input[Int]): ResumableIter[Int, EphemeralStream[Int]] = 
	s( el = e => {
	    //println("i "+i+" e "+e)
	    Done((iTo(i, e), Cont(step(i + 1))), IterV.Empty[Int])
	  },
	  empty = Cont(step(i)),
	  eof = Done((EphemeralStream.empty, Cont(error("Shouldn't call cont on eof"))), IterV.EOF[Int])
	)

      Cont(step(i))
    }

    val enum = (i: Int) => enumToMany(sum[Int])(f(i))

    // 1, 2, 3, 4 only, the to is ignored
    val (res, cont) = enum(1)(l.iterator).run
    assertEquals(10, res)
    assertTrue("should have been done", isDone(cont))

    // -1->1 0, 0->2 3, 1->3 6, 2->4 9 
    val (res2, cont2) = enum(-1)(l.iterator).run
    assertEquals(18, res2)
    assertTrue("2 should have been done", isDone(cont2))

  }

  def testEnumToManyExhaust = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = enumToMany(sum[Int])( mapTo( (i: Int) => El(iTo(1, i)) ) )(i).run
    assertEquals(20, res)
    assertTrue("should have been done", isDone(cont))
  }

  def testEnumToManyEOFFromMap = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = enumToMany(sum[Int])( mapTo( (i: Int) => EOF[EphemeralStream[Int]] ) )(i).run
    assertEquals(0, res)
    assertTrue("should have been done", isDone(cont))

    val rest = i.take(4).toSeq
    //println(rest)
    assertEquals("Should still have had items in the list", 9, rest.sum)
  }

  /**
   * Make sure it doesn't soe in the loop
   */ 
  def testEnumToManySOE = {
    val i = List[Long](1,2,3,4,5).iterator
    
    val (res, cont) = enumToMany(sum[Long])( mapTo( (_:Long) => El(lTo(1L, 100000L)) ) )(i).run
    assertEquals(25000250000L, res)
    assertTrue("should have been done", isDone(cont))
  }

  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // force a restart at magic number 1
    val sum: ResumableIter[Long,Long] = {
      def step(acc: Long)( s : Input[Long] ) : ResumableIter[Long, Long] =
	s( el = e => {
	    val nacc = acc + e
	    if (nacc == 25000050001L)
	      Done((nacc, Cont(step(nacc))), IterV.Empty[Long])
	    else
	      Cont(step(nacc))
	  },
	  empty = Cont(step(acc)),
	  eof = Done((acc, Done(acc, IterV.EOF[Long])), IterV.EOF[Long])
	)

      Cont(step(0))
    }

    val (res, cont) = enumToMany[Long, Long, Long](sum)( mapTo( (_:Long) => El(lTo(1L, 100000L)) ) )(i).run
    assertEquals(25000050001L, res)
    assertFalse("should not have been done", isDone(cont))

    println("cont is " + cont)

    val canRestart = (cont.asInstanceOf[ResumableIter[Long, Long]]).run //(i).run

    println("after eval is " + canRestart)
    val (res2, cont2) = canRestart//(extract(canRestart).get, canRestart)
    assertEquals(25000250000L, res2)
    assertTrue("should have been done", isDone(cont2))
  }


  def testEveryItem = {
    val source = List[Long](1,4,7,10,13).iterator

    // force a restart every entry
    val echo: ResumableIter[Long,Long] = {
      def step( s : Input[Long] ) : ResumableIter[Long, Long] =
	s( el = e => {
//	  println("got "+e)
	    //if (e > 2) {
	      Done((e, Cont(step)), IterV.Empty[Long])
	    //} else Cont(step)
	    //
	  },
	  empty = Cont(step),
	  eof = Done((0, Done(0, IterV.EOF[Long])), IterV.EOF[Long])
	)

      Cont(step)
    }

    val (origres, origcont) = enumToMany[Long, Long, Long](echo)( mapTo{ (x:Long) => println("evaled to "+x);El(lTo(x, x + 2L)) })(source).run

    var res = origres
    var iter : ResumableIter[Long, Long] = origcont.asInstanceOf[ResumableIter[Long, Long]]

    var count = 1 // already run once

    for( i <- 1 to 15 ) {
//      println("loop !!!! "+i)
      assertEquals(i, res)
      val cres : ResumableIter[Long, Long] = iter.eval
      if (isDone(cres)){
	res = extract(cres).get
	iter = extractCont(cres)
      } else {
//	println(">>><<<<>>>>><<<<")
	count += 1
	// run on the source - as we need more data
	val (nres, ncont) = cres(source).run
	res = nres
	iter = ncont.asInstanceOf[ResumableIter[Long, Long]]

	if (count == 6) {
	  assertEquals("should have pushed the eof", 15, i)
	}
      }      
    }

    assertEquals(0, res)
    assertTrue("should have been done", isDone(iter))
    assertTrue("should have been eof", isEOF(iter))
    assertFalse("source should be empty", source.hasNext)
    assertEquals("should have reset 6 times - once for start and once for EOF ", 6, count)
  }

  */ 

  def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R] = {
    val empty = () => EphemeralStream.empty

    def loop( i: ResumableIter[A,R], s: () => EphemeralStream[A] ):
      (ResumableIter[A,R], () => EphemeralStream[A]) = {
      var c: ResumableIter[A,R] = i
      var cs: EphemeralStream[A] = s() // need it now
//println("loopy "+ isDone(c)+ " " +cs.isEmpty)
      while(!isDone(c) && !cs.isEmpty) {
//	println("doing a loop "+c)
	val (nc, ncs): (ResumableIter[A,R], EphemeralStream[A]) = c.fold(
	  done = (a, y) => (c, s()),// send it back, shouldn't be possible to get here anyway due to while test
	  cont = 
	    k => {
//	      println("got cont")
	      val head = cs.head() // if used in El it captures the byname not the value
	      (k(IterV.El(head)), cs.tail())
	    }
	    )
	c = nc
	cs = ncs
      }
      (c, () => cs)
    }

//    class BounceIt() extends 

    def contk( k: scalaz.Input[A] => ResumableIter[A,R],  i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, R] = {
      if (!s().isEmpty) {
	val (ni, ns) = loop(i, s)
	println("empty against the s "+ni + " " + ns().isEmpty)
	//if (isDone(ni)) 
	next(ni, ns, toMany) // bad - should let a done exit early
      } else
	Cont((x: Input[E]) => 
	  x( el = e => {
	    println("got a cont x el e "+e)
	    toMany.fold (
	      done = (a, y) => {
		val (e1, nextContR) = a
		val nextCont = nextContR.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
		error("Unexpected State for enumToMany - Cont but toMany is done")		     	
	      },
	      cont = y => {

		/**/		      
		println("and then I was here "+
			x(el = e => e.toString, 
			  empty = "I'm empty ",
			  eof = "I'm eof"))

		val afterNewCall = y(x)
		println("and then " + afterNewCall)

		/*
		 * So the first chunk is the only chunk, we have the first cont - should be done onthe cont?
		 */ 

		afterNewCall.fold(
		  done = (nextContPair, rest) => {
		    println("was done wern't it")
		    val (e1, nextCont) = nextContPair
		    val nextContR = nextCont.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
		    if (isEOF(afterNewCall)) {
		      println("after is eof")
		      next(k(IterV.EOF[A]), empty, nextContR)
		    } else {
		      if (e1.isEmpty) {
			println("empty on nextcontr")
			next(k(IterV.Empty[A]), empty, nextContR)
		      }
		      else {
			val h = e1.head()
			println("some data after all "+h)
			// should feed that back

			next(k(IterV.El(h)), e1.tail, nextContR)

			/*			      val r = k(IterV.El(e1.head()))
			 if (isDone(r))

Done((el.head(), next(k(IterV.El(e1.head())), e1.tail, nextContR)), IterV.Empty[E])*/
		      }
		    }
		  },
		  cont = k1 => {
		    println("conted after here")
		    next(k(IterV.Empty[A]), empty, afterNewCall)
		  }
		)
	      }
	    )
	  },
	    empty = {
	      println("empty <--<--<--")
	      next(k(IterV.Empty[A]), empty, toMany)
	    },
	    eof = {
	      println("eof <--<--<--")
	      next(k(IterV.EOF[A]), empty, toMany)
	    }
	  ))

    }

    def doneWith(a: (R, ResumableIter[A,R]), y: Input[A], i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]], internalEOF: Boolean ): ResumableIter[E, R] = {

      // println(" y is "+y) 

      val (res, nextCont) = a
      println("res is "+ res) 

      val returnThis : ResumableIter[E, R] = 
	if ((isDone(nextCont) && isEOF(nextCont)) ||
	    (isDone(toMany) && isEOF(toMany))     || // either eof then its not restartable
	    (EOF.unapply(y) && !internalEOF )  // or the source is out of elements
	  ) { 
	  if (EOF.unapply(y)) {
	    println("it was eof from the run weren't it "+internalEOF)
	  }

	  Done((res, Done(res, IterV.EOF[E])), IterV.EOF[E])
	} else {
	  // there is a value to pass back out
	  println("iz here")
	  Done((res, 
		{
		  val cont = () => next(nextCont.asInstanceOf[ResumableIter[A,R]], s, toMany, true)
		  // really don't want to eval this now, lazy ftw
		  val n = Cont( (i: Input[E]) => {
		    println("about to next from up in here "+ s().isEmpty)
		    cont()
		  })
		  
		  //n
		  if (s().isEmpty) {
		    println("got here with s().empty")
		    //new RuntimeException("Da fuck?!!").printStackTrace()

		    // there is no more data saved up
		    n//Done((res, n), IterV.Empty[E])
		  } else {
		    println("non - empty")
		    // still data to process
		    cont()
		  }
		}), IterV.Empty[E])
	}

      if (EOF.unapply(y) && !internalEOF) {
	// signal the end here to toMany, don't care about result
	toMany.fold(done= (a1, y1) => false,
		    cont = k => {
		      k(IterV.EOF[E]); false
		    })
      }
      
      returnThis
      
    }
      
//    @scala.annotation.tailrec
    def next( i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]], internalEOF: Boolean = false ): ResumableIter[E, R] =
      
	i.fold(
	done = (a, y) => doneWith(
	  // oh for recursive types
	  a.asInstanceOf[(R, ResumableIter[A, R])], y, i, s, toMany, internalEOF),
	cont = 
	  k => {
	    println("Fucksake")
	    contk(k, i, s, toMany)
	  }
      )

    next(dest, empty, toMany)
  }


  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) EphemeralStream.empty else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) EphemeralStream.empty else EphemeralStream.cons(lower, iTo(lower + 1, upper))

}
