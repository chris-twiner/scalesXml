package scales.xml.aalto

import com.fasterxml.aalto._
import AsyncXMLStreamReader.EVENT_INCOMPLETE
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
class AsyncParser[T : PullTypeEvidence]( val parser : AsyncXMLStreamReader, val channel : ReadableByteChannel, val optimisationStrategy : MemoryOptimisationStrategy[_ <: OptimisationToken] = defaultOptimisation, private val bytePool : Pool[ByteBuffer] ) extends CloseOnNeed with DocLike {

  val feeder = parser.getInputFeeder
  val buffer = bytePool.grab

  val token = optimisationStrategy.createToken

  // to capture the miscs.
  var docImpl = EmptyDoc()
  def prolog = docImpl.prolog
  def end = docImpl.end

  /**
   * Only closes the input feeder, the user is reponsible for "closing" the parser
   */ 
  protected def doClose = { 
    bytePool.giveBack(buffer)
    feeder.endOfInput
  }
}

/**
 * NOTE this will be removed as soon as I have a workable solution to keeping buffers around
 */ 
class NeedsRingBuffer(msg : String) extends Exception(msg)

import scalaz._
import scalaz.IterV._

final class AsyncParserEnumerator extends Enumerator[AsyncParser] {

  def bytes[E](parser : AsyncParser[E]) : (Array[Byte], Int) = {
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

  val incompletePull : PullType = Left(Text("I am incomplete"))

  def apply[E,A](parser: AsyncParser[E], i: IterV[E,A]): IterV[E,A] = {
    @annotation.tailrec
    def intern[E,A](parser: AsyncParser[E], i: IterV[E,A], bytes : AsyncParser[E] => Boolean, depth : Int, started : Boolean): IterV[E,A] = {
      def pump(k : Input[E] => IterV[E,A]) : IterV[E,A] = {
	// don't have to re-read, let it push what it has
	if ((depth == -1) && !started) {
	  val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser.parser, parser.strategy, paresr.token, parser.prolog, depth){ x =>
	    if (x == EVENT_INCOMPLETE)
	      incompletePull
	    else
              error("Got an unexpected event type " + x +" cannot proceed.") }

	  if (num == EVENT_INCOMPLETE) {
	    // let the iter attempt to deal
	    intern(parser, k(Empty[E]), odepth)
	  } else {
	    var nprolog = oprolog

	    if (event.isLeft && (event.left.get eq PullUtils.dtdDummy)) {
              nprolog = oprolog.copy(dtd = Some(
		DTD("", "", "") // DTD has funnyness TODO find out what it looks like
              ))
	    }

	    if (odepth == -1) {
	      nprolog = nprolog.copy(misc = nprolog.misc :+ PullUtils.getMisc(event, "prolog"))
	    }

	    parser.docImpl = parser.docImpl.copy( prolog = nprolog )

	    if (odepth > -1) {
	      // we have a real event
	      intern(parser, k(El(event.asInstanceOf[E])), odepth)
	    } else // empty
	      intern(parser, k(Empty[E]), odepth)
	  }

	} else {
	  val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser.parser, parser.strategy, paresr.token, parser.prolog, depth){ x =>
	    if (x == EVENT_INCOMPLETE)
	      incompletePull
	    else
              error("Got an unexpected event type " + x +" cannot proceed.") }

	  if (num == EVENT_INCOMPLETE) {
	    // let the iter attempt to deal
	    intern(parser, k(Empty[E]), odepth)
	  } else {

	val next = pump / prolog / end
	intern(parser, k(El(next.asInstanceOf[E])), depth)
	}
      }

      i match {
	//case _ if iter.isEmpty => i // can't model this without requesting it ?
	case Done(acc, input) => i
	case Cont(k) =>
          if (parser.feeder.needMoreInput) {
	    // attempt to pump out
	    val (ar, read) = bytes
	    read match {
	      case -1 => 
		parser.close
		intern(parser, k(EOF[E]), depth)
	      case 0 =>
		intern(parser, k(Empty[E]), depth)
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
    inter(parser, i, 
	  if (parser.buffer.hasArray)
	    bytes(parser) _
	  else {
	    // perfectly valid for a mem mapped to be huge, in which case, we would have grief ?
	    val d = direct(Array[Byte].ofDim(parser.buffer.capacity))
	    d(parser) _
	  }, -1
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
