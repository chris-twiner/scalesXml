package scales.xml.aalto

import com.fasterxml.aalto._
import AsyncXMLStreamReader.EVENT_INCOMPLETE
import javax.xml.stream.XMLStreamConstants.END_DOCUMENT
import scales.xml._
import scales.utils._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

sealed trait PullTypeEvidence[T]

object PullTypeEvidence {
  implicit val pullOnly : PullTypeEvidence[PullType] = new PullTypeEvidence[PullType]{}
}

/**
 * Represents an asnych parser.  T is a dummy for the enumerator, only PullType is supported.
 * Callers MUST ensure that close is called in the case of an exception as well as the end of processing, failing to do so will cause unnecessary ByteBuffer creation. 
 */ 
abstract class AsyncParser[-T]( val parsers : Pool[AsyncXMLStreamReader], val channel : ReadableByteChannel, private val bytePool : Pool[ByteBuffer] )(implicit xmlVersion : XmlVersion, ev : PullTypeEvidence[T] ) extends CloseOnNeed with DocLike {

  type Token <: OptimisationToken

  val strategy : MemoryOptimisationStrategy[Token]

  val feeder : AsyncInputFeeder
  val buffer = bytePool.grab

  val token : Token

  val parser : AsyncXMLStreamReader

  // to capture the miscs.
  private[this] var docImpl = EmptyDoc()

  def copyProlog(p : Prolog) { 
    docImpl = docImpl.copy( prolog = p )
  }

  def addPrologMisc(m : PullType) { 
    copyProlog( 
      prolog.copy(
	misc = prolog.misc :+ PullUtils.getMisc(m, "prolog") 
      )
    )
  }

  def addEndMisc(m : PullType) { 
    docImpl = docImpl.copy( 
      end = end.copy( 
	misc = end.misc :+ PullUtils.getMisc(m, "endMisc") 
      )
    )
  }

  def prolog = docImpl.prolog
  def end = docImpl.end

  /**
   * Only closes the input feeder, the user is reponsible for "closing" the parser
   */ 
  protected def doClose = { 
    bytePool.giveBack(buffer)
    feeder.endOfInput
    parsers.giveBack(parser)
  }
}

object AsyncParser {
  /**
   * Creates a parser based on the input channel provided
   */
  def apply[T, TokenT <: OptimisationToken]( channel : ReadableByteChannel, optimisationStrategy : MemoryOptimisationStrategy[TokenT] = defaultOptimisation, bytePool : Pool[ByteBuffer] = defaultBufferPool, parsers : Pool[AsyncXMLStreamReader] = AsyncStreamReaderPool )( implicit xmlVersion : XmlVersion, ev : PullTypeEvidence[T] ) : AsyncParser[T] = new AsyncParser[T](parsers, channel, bytePool){
    type Token = TokenT
    val strategy = optimisationStrategy
    
    import PullUtils.weAreInAParser
    val token = strategy.createToken
    val parser = parsers.grab
    val feeder = parser.getInputFeeder
  }
}

/**
 * NOTE this will be removed as soon as I have a workable solution to keeping buffers around
 */ 
class NeedsRingBuffer(msg : String) extends Exception(msg)

import scalaz._
import scalaz.IterV._

final class AsyncParserEnumerator extends Enumerator[AsyncParser] {

  def jbytes[E](parser : AsyncParser[E]) : (Array[Byte], Int) = {
    val read = parser.channel.read(parser.buffer)
    (parser.buffer.array, read)
  }

  def direct[E](to : Array[Byte])(parser : AsyncParser[E]) : (Array[Byte], Int) = {
    val read = parser.channel.read(parser.buffer)
    if (read != -1) {
      parser.buffer.get(to)
    }
    (to, read)
  }

  val incompOrEnd : PullType = Left(Text("I am incomplete or doc end"))

  def apply[E,A](parser: AsyncParser[E], i: IterV[E,A]): IterV[E,A] = {
    //@annotation.tailrec
    def intern[E,A](parser: AsyncParser[E], i: IterV[E,A], bytes : () => (Array[Byte], Int), depth : Int, started : Boolean): IterV[E,A] = {

      /**
       * when the data is empty in prolog or endmisc send back a continuation to process more, when in the root elem itself then the iteratee i should deal with it
       */ 
      def pumpInMisc : IterV[E, A] =
	Cont(_ => intern(parser, i, bytes, depth, started))

      val eventHandler = (x : Int) => {
	if (x == EVENT_INCOMPLETE || x == END_DOCUMENT)
	  incompOrEnd
	else
          error("Got an unexpected event type " + x +" cannot proceed.") 
      }

      def pump(k : Input[E] => IterV[E,A]) : IterV[E,A] = {
	// don't have to re-read, let it push what it has
	if (depth == -1) {
	  // miscs

	  val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser.parser, parser.strategy, parser.token, parser.prolog, depth)(eventHandler)

	  if (oprolog != parser.prolog) {
	    // doc start
	    parser.copyProlog( oprolog ) 
	  }
	  
	  if (num == END_DOCUMENT) {
	    // EOF - let the iter deal 
	    intern(parser, k(EOF[E]), bytes, odepth, started)
	  } else if (num == EVENT_INCOMPLETE) {
	    pumpInMisc
	  } else if (odepth == -1) {
	    // still misc
	    
	    if (event.isLeft && (event.left.get eq PullUtils.dtdDummy)) {
	      parser.copyProlog( parser.prolog.copy(dtd = Some(
		DTD("", "", "") // DTD has funnyness TODO find out what it looks like
              )))
	    } else {
	      if (!started)
		parser.addPrologMisc(event)
	      else
		parser.addEndMisc(event)	      
	    }

	    pumpInMisc
	  } else {
	    // pump actual first event - yay !!, next depth -1 is endmisc
	    intern(parser, k(El(event.asInstanceOf[E])), bytes, odepth, true)
	  }
	} else {
	  // 2nd > events
	  val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser.parser, parser.strategy, parser.token, parser.prolog, depth)(eventHandler)

	  if (num == END_DOCUMENT) {
	    // EOF - let the iter deal -- should not occur here though, only when depth == -1
	    intern(parser, k(EOF[E]), bytes, odepth, started)
	  } else if (num == EVENT_INCOMPLETE) {
	    // let the iter attempt to deal
	    intern(parser, k(IterV.Empty[E]), bytes, odepth, started)
	  } else {
	    // actual data present, odepth -1 is looked at for the next iter
	    intern(parser, k(El(event.asInstanceOf[E])), bytes, odepth, started)
	  }
	}
      }

      i match {
//	case _ if !parser.parser.hasNext => i // doesn't matter if its EOF or not
	case Done(acc, input) => i
	case Cont(k) =>
          if (parser.feeder.needMoreInput) {
	    // attempt to pump out
	    val (ar, read) = bytes()
	    read match {
	      case -1 => 
		parser.closeResource
		intern(parser, k(EOF[E]), bytes, depth, started)
	      case 0 =>
		intern(parser, k(IterV.Empty[E]), bytes, depth, started)
	      case _ =>
		// feed data in
		parser.feeder.feedInput(ar, 0, read)
		
		if (parser.feeder.needMoreInput) {
		  // we try again...? in the meantime this is death, requires a buffer ring due to feedInput's docs
		  throw new NeedsRingBuffer("Was pumped, but needs more to do anything, which requires us to add a ring")
		} else {
		  // we can pump
		  pump(k)
		}
	    }
	    
	  } else {
	    pump(k)
	  }
      }
    }
    intern(parser, i, 
	  if (parser.buffer.hasArray)
	    () => jbytes(parser)
	  else {
	    // perfectly valid for a mem mapped to be huge, in which case, we would have grief ?
	    val d = direct(Array.ofDim[Byte](parser.buffer.capacity)) _
	    () => d(parser)
	  }, -1, false
	)
  }
}

/*
 *

case class AsyncInput(event, parser)

prolog = IterV[AsyncInput, Prolog]
// finishes after last end elem, can be combined with onDone etc
main = IterV[AsyncInput, PullType] >> onDone
// this can kick in
end = IterV[AsyncInput, EndMisc]

for {
 p <- prolog
 m <- main
 e <- end
 } yield DocLike { p, e }

the iteratees must all share

byte[] -> can't process any more (needMoreInput == false) -> Done Empty?
       -> can do more (needMoreInput true) -> Cont
       -> enough -> EOL

// wrapper for [_]
trait AsyncParser[T] {
  parser, AsyncInputFeeder, ReadableByteChannel, buffer etc
} // as this owns the buffer and channel we can contain the flow/not reading any more than needed

Enumerator[AsyncParser] // turns the asyncparser input into event (and "loops" until needed".

    @annotation.tailrec
    def apply[E,A](iter: Iterator[E], i: IterV[E,A]): IterV[E,A] = i match {
      case _ if iter.isEmpty => i
      case Done(acc, input) => i
      case Cont(k) =>
	val x : E = iter.next//.asInstanceOf[E]
      apply(iter, k(El(x)))
    }



*/
