package scales.xml.parser.pull.aalto

import scalaz.Free.Trampoline
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.{Iteratee, IterateeT, StepT}
import scalaz.iteratee.Iteratee.{iterateeT, peek}
import scalaz.iteratee.StepT.Done
import scalaz._
import Scalaz._
import junit.framework.Assert.assertTrue
//import scales.utils.iteratee.functions.referenceDedup
import scales.utils.trampolineIteratees._
import scales.utils.iteratee.functions.ResumableIterOps

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

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import EphemeralStream.emptyEphemeralStream

  import scales.utils.{resource => sresource}
  import scalaz.iteratee.Iteratee.{cont, done, elInput, empty, foldM, iterateeT, repeat}
  import DangerousIterateeImplicits._ // toRunEval
  import TestIteratees._

  val smallBufSize = 10
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = smallBufSize )

  import serializers._


  type SerialIterT = IterateeT[PullType, TheF, (XmlOutput, Option[Throwable])]

  import java.nio.charset.Charset
  import scales.utils.{io, resources}
  import resources._

  /**
   * returns the cont and drops the input parameter or returns Done
   */
  trait EvalW[WHAT, F[_],RETURN] {

    val orig : IterateeT[WHAT, F, RETURN]

    def evalw(implicit F: Monad[F]) : IterateeT[WHAT, F, RETURN] =
      iterateeT(
        F.bind(orig.value){s => s(done = (x, y) => F.point( Done(x,y)),
        cont = k => orig.value)}
      )
  }

  implicit def toEvalw[WHAT, F[_], RETURN]( i : IterateeT[WHAT, F, RETURN] ) = new EvalW[WHAT, F, RETURN] {
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

    val iter: IterateeT[PullType, TheF, Option[PullType]] =
      for {
        _ <- peek[PullType, TheF]
        _ <- peek[PullType, TheF]
        _ <- peek[PullType, TheF]
        _ <- peek[PullType, TheF]
        _ <- peek[PullType, TheF]
        i <- evalWith((p: PullType) => {
          p
        })(implicitly[Applicative[TheF]])
        j <- dropWhile((p: PullType) => {
          p.fold(x => !x.isInstanceOf[Elem], y => false)
        })(implicitly[Monad[TheF]])
      } yield j
    
    val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(parser))
    val wrapped = new ReadableByteChannelWrapper(channel, true, tinyBuffers)

    val p =
      for {
        r <- (enumeratee &= dataChunkerEnumerator(wrapped)).run
        (e,cont) = r
      } yield {
        assertTrue("Should be defined", e.isDefined)
        assertEquals("{urn:default}DontRedeclare", e.get.left.get.asInstanceOf[Elem].name.qualifiedName)
        parser.closeResource
        wrapped.closeResource
      }

    p run
  }

  def testSimpleLoadAndFold(): Unit =
    doSimpleLoadAndFold[TheF]{
      (p, iter, wrapped) => 
      val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(p))
      for {
        r <- (enumeratee &= dataChunkerEnumerator[DataChunk, TheF](wrapped)).run
        (e,cont) = r
      } yield e
    } run

  def doSimpleLoadAndFold[F[_]: Monad](test: (AsyncParser, IterateeT[PullType, F, List[String]], ReadableByteChannelWrapper[DataChunk ]) =>  F[List[String]] ) : F[Unit] = {
    import scales.utils.iteratee.functions._
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val parser = AsyncParser()

    val ns = Namespace("urn:default")

    val iter = foldOnDoneIter( List[String](), 
      onQNames[F](List(ns("Default"), "NoNamespace"l,"DontRedeclare"l))){
        (l, qmatch) => qmatch._2.  // its empty as it was eof
          map(p => qname(p.tree) :: l).getOrElse( l )
      }

    val wrapped = new ReadableByteChannelWrapper(channel)
    for {
      e <- test(parser, iter, wrapped)
    } yield {
      assertEquals(2, e.size)
      assertTrue("Channel was not closed", !channel.isOpen)
      e match {
        case List("DontRedeclare", "DontRedeclare") => ()
        case _ => fail("got " + e)
      }
    }
  }

  def testSimpleLoadAndFoldAsync =
    // should have collected all anyway
    doSimpleLoadAndFold[Trampoline]{
      (p, iter, wrapped) => 
      val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(p))
      (enumeratee &= dataChunkerEnumerator[DataChunk, Trampoline](wrapped)).run map {
        _._1
      }
    } run
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
            val h = st.headOption.get
            res = res :+ h
            st = st.tailOption.get
          }
        },
        empty = {nexted += 1;()},
        eof = {nexted += 1;()}
      )
    }
    
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertTrue("we should have more nexted then zeroed - due to boundaries on the available data", randomChannelOrig.zeroed + 1 < nexted)
    assertTrue("Random channel should have been closed", !randomChannelOrig.isOpen)
  }

  // using the parser and the parse iteratee
  def testRandomAmountsParse = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()

    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize).wrapped

    val parser = AsyncParser()

    import scales.utils.trampolineIteratees._
    val r =
      ( foldIM[DataChunk, (ResumableIter[DataChunk, EphemeralStream[PullType]], DataChunk, Vector[PullType])](
        (_, triple ) => {
          val (c, b, res) = triple

          def input(b: DataChunk) =
            if (b.isEOF)
              Eof[DataChunk]
            else
              if (b.isEmpty)
                Empty[DataChunk]
              else
                Element(b)

          def nextC(step: ResumableStep[DataChunk, EphemeralStream[PullType]], b: DataChunk) =
            step(
              done = (a, y) => { // if eof
                val (e, cont) = a

                var nres = res
                var st = e
                while(!st.isEmpty) {
                  val h = st.headOption.get
                  nres = nres :+ h
                  st = st.tailOption.get
                }

                (nres, cont.asInstanceOf[ResumableIter[DataChunk, EphemeralStream[PullType]]])
              },
              cont = k => {
                // need to push the next chunk
                val chunk = input(b)
                (res, k(chunk))
              }
            )

          c.value.map {
            step =>
              val nextb =
                // if its done we have to pump
                if (!randomChannel.isClosed && !isDoneS(step)) {
                  randomChannel.nextChunk
                } else {
                  b
                }
              val (nres, nextc) = nextC(step, nextb)
              (nextc, nextb, nres)
          }
      })((AsyncParser.parse[TheF](parser),EmptyData, Vector.empty[PullType]), stopOn = _._2 == EOFData)
        &= repeat[DataChunk, TheF](EmptyData)) run

    val p =
      for {
        bits <- r
        ((c, data, res), cont) = bits
        cStep <- c.value
      } yield {
        val s = asString(res.iterator : Iterator[PullType])
        assertEquals(s, str)

        assertTrue("Cont should have been eof", isEOFS(cStep))
        assertTrue("Parser should have been closed", parser.isClosed)

        assertTrue("Channel should have been closed", randomChannel.isClosed)
      }

    p run
  }


  /**
   * Hideously spams with various sizes of data, and more than a few 0 lengths.
   *
   * The aim is to test the proverbial out of the async code.  It should be able to handle being called with a single byte repeatedly.
   */
  def testRandomAmounts = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    // idIteratees works if you don't eval, the old evalw didn't force the stack.
    import ioIteratees._

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()

    val randomChannel = new RandomChannelStreamWrapper(stream, smallBufSize)

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter[TheF]( strout , doc )

    val magicPull = FullChunk(Array.ofDim[Byte](0))

    val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(parser))
    val wrapped = new ReadableByteChannelWrapper(randomChannel, true, tinyBuffers)

    /*
     * the random channel does every 10, every 6 forces it back but allows
     * a fair chunk of Empty -> Conts followed by data
     */
    val readableByteChannelEnumerator = new AsyncDataChunkerEnumerator[DataChunk, TheF](wrapped, 6 )

    val cstable = (enumeratee &= readableByteChannelEnumerator).evalw

    var c = cstable

    import scales.utils.iteratee.monadHelpers._
    var count = 0
/*
    do {
      c = (c &= dataChunkerEnumerator(wrapped)).evalw
      count += 1
      done = (for {
        r <- isDone(c)
      } yield r).runIt
    } while (!done) */

    val itr =
      repeatUntilM((cstable, false, 0)) {
        triple =>
          val (c, stop, count) = triple
          if (!stop) {
            F.bind(c.value) { cstep =>
              val wasEmpty = isEmptyS(cstep)

              val cont = if (isDoneS(cstep)) extractContS(cstep) else c
              val newc = (cont &= dataChunkerEnumerator(wrapped)).evalw
              newc.value.map { step =>
                (newc, isEOFS(step), if (isDoneS(cstep)) count else count + 1)
              }
            }
          } else
            F.point(triple)
      }(_._2)

    val p =
      for {
        r <- itr
        // stopped on EOF for the cont - but never pushes the eof to the parser
        ((cont, _, count), remainingCont) = r
        //step <- c.value
        step <- cont.value
      } yield {

/*        if (randomChannel.zeroed > 0) {
          assertTrue("There were " + randomChannel.zeroed + " zeros fed but it never left the evalw", count > 0)
        }
*/
        step(
          done = (a, i) => {
            val ((out, thrown), cont) = a
            assertFalse("shouldn't have thrown", thrown.isDefined)

            assertTrue("should have been auto closed", closer.isClosed)
            if (str != strout.toString) {
              println
              println(strout.toString)
            }
            assertEquals(str, strout.toString)

            // the output stream is closed, the input stream and parser is closed
            assertTrue("should have been EOF", i.isEof)
          },
          cont = f => fail("Should have been done")
        )
      }

    p.runIt

    assertTrue("Parser should have been closed ", parser.isClosed)
    assertTrue("Wrapper should have been closed ", wrapped.isClosed)
  }

  def testSimpleLoadSerializingMisc = {
    import idIteratees._
    val url = sresource(this, "/data/MiscTests.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc) // especially needed here as we may have whitespace which isn't collected.

    val channel = Channels.newChannel(url.openStream()).wrapped

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter[Id]( strout , doc )

    val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(parser))
    val ((out, thrown), cont) = (enumeratee &= dataChunkerEnumerator(channel)).runEval

    // we can swallow the lot, but endmiscs don't know there is more until the main loop, which needs evaling
    assertFalse( "shouldn't have thrown", thrown.isDefined)

    assertTrue("parser should have been closed", parser.isClosed)
    assertTrue("should have been auto closed", closer.isClosed)
    assertTrue("channel should have been auto closed", channel.isClosed)


    assertEquals(str, strout.toString)
  }

  def testSimpleLoad = {
    import idIteratees._
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val channel = Channels.newChannel(url.openStream())

    val iter = evalAll(Left(Text("")), (p : PullType) => {
//      println(p);
      p} )

    val parser = AsyncParser()

    val enumeratee = enumToMany(iter.toResumableIter)(AsyncParser.parse(parser))
    val (e, cont) = (enumeratee &= dataChunkerEnumerator(channel.wrapped)).run

    assertEquals("{urn:default}Default", e.right.get.name.qualifiedName)
    assertTrue("The parser should have been closed", parser.isClosed)
    assertTrue("The channel should have been closed", !channel.isOpen)

  }

  def testSimpleLoadSerializing =
    doSimpleLoadSerializing(Channels.newChannel(_).wrapped)

  def doSimpleLoadSerializing(streamToChannel: java.io.InputStream => DataChunker[DataChunk]) = {
    import idIteratees._
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val channel = streamToChannel(url.openStream())

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter[Id]( strout , doc )

    val enumeratee = enumToMany(iter.toResumableIter)(parser.iteratee)
    val ((out, thrown), cont) = (enumeratee &= dataChunkerEnumerator(channel)).run

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