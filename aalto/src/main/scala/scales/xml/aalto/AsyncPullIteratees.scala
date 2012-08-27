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
abstract class AsyncParser[-T]( val parsers : Pool[AsyncXMLInputFactory], val channel : ReadableByteChannel, private val bytePool : Pool[ByteBuffer] )(implicit xmlVersion : XmlVersion, ev : PullTypeEvidence[T] ) extends CloseOnNeed with DocLike {

  type Token <: OptimisationToken

  val strategy : MemoryOptimisationStrategy[Token]

  val feeder : AsyncInputFeeder
  val buffer = bytePool.grab

  protected val pf : AsyncXMLInputFactory

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
    parser.close
    parsers.giveBack(pf)
  }
}

object AsyncParser {
  /**
   * Creates a parser based on the input channel provided
   */
  def apply[T, TokenT <: OptimisationToken]( channel : ReadableByteChannel, optimisationStrategy : MemoryOptimisationStrategy[TokenT] = defaultOptimisation, bytePool : Pool[ByteBuffer] = defaultBufferPool, parsers : Pool[AsyncXMLInputFactory] = AsyncXMLInputFactoryPool )( implicit xmlVersion : XmlVersion, ev : PullTypeEvidence[T] ) : AsyncParser[T] = new AsyncParser[T](parsers, channel, bytePool){
    type Token = TokenT
    val strategy = optimisationStrategy
    
    import PullUtils.weAreInAParser
    val token = strategy.createToken

    val pf = parsers.grab

    val parser = pf.createAsyncXMLStreamReader()
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

  def reset {stack = -1}
  var stack = -1
  def inc { stack += 1; 
	   if (stack > 0)
	   println("inced " + stack) }
  def dec { stack -= 1 }

  def jbytes[E](parser : AsyncParser[E]) : (Array[Byte], Int) = {
    parser.buffer.clear()
    val read = parser.channel.read(parser.buffer)
    (parser.buffer.array, read)
  }

  def direct[E](to : Array[Byte])(parser : AsyncParser[E]) : (Array[Byte], Int) = {
    parser.buffer.clear()
    val read = parser.channel.read(parser.buffer)
    if (read != -1) {
      parser.buffer.get(to)
    }
    (to, read)
  }

  val incompOrEnd : PullType = Left(Text("I am incomplete or doc end"))


  val eventHandler = (x : Int) => {
    if (x == EVENT_INCOMPLETE || x == END_DOCUMENT)
      incompOrEnd
    else
      error("Got an unexpected event type " + x +" cannot proceed.") 
  }

  private[this] class ContMe[E,A]( val parser: AsyncParser[E], val i: IterV[E,A], val bytes : () => (Array[Byte], Int), val depth : Int, val started : Boolean) extends Exception

  def apply[E,A](parser: AsyncParser[E], i: IterV[E,A]): IterV[E,A] = {

    def pump[E](parser: AsyncParser[E], depth : Int, started : Boolean) : Option[(Input[E], Int, Boolean)] = {
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
	  Some((EOF[E], odepth, started))
	} else if (num == EVENT_INCOMPLETE) {
	  None//pumpInMisc
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

	  None
	} else {
	  // pump actual first event - yay !!, next depth -1 is endmisc
	  Some((El(event.asInstanceOf[E]), odepth, true))
	}
      } else {
	// 2nd > events
	val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser.parser, parser.strategy, parser.token, parser.prolog, depth)(eventHandler)

	if (num == END_DOCUMENT) {
	  // EOF - let the iter deal -- should not occur here though, only when depth == -1
	  Some((EOF[E], odepth, started))
	} else if (num == EVENT_INCOMPLETE) {
	  // let the iter attempt to deal
	  Some((IterV.Empty[E], odepth, started))
	} else {
	  // actual data present, odepth -1 is looked at for the next iter
	  Some((El(event.asInstanceOf[E]), odepth, started))
	}
      }
    }

    @annotation.tailrec
    def intern[E,A](parser: AsyncParser[E], i: IterV[E,A], bytes : () => (Array[Byte], Int), depth : Int, started : Boolean): IterV[E,A] = 
      i match {
	case _ if parser.isClosed => 
	  //println("closed and got " +i)
	  i // either external or due to -1 from bytes
	case Done(acc, input) => i
	case Cont(k) =>
	  //println("cont")

          if (parser.feeder.needMoreInput) {
	    
	    // attempt to pump out
	    val (ar, read) = bytes()
	    //println("more "+ar+", "+read)
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
		  val o = pump(parser, depth, started)
		  o match {
		    case Some((in, odepth, ostarted)) => 
		      intern(parser, k(in), bytes, odepth, ostarted)
		    case _ => 
		      throw new ContMe[E,A](parser, i, bytes, depth, started)
		  }
		}
	    }
	    
	  } else {
	    //println("enough")
	    val o = pump(parser, depth, started)
	    o match {
	      case Some((in, odepth, ostarted)) => 
		intern(parser, k(in), bytes, odepth, ostarted)
	      case _ => 
		throw new ContMe[E,A](parser, i, bytes, depth, started)
	    }
	  }
	}

    def callInt( x : ContMe[E,A] ) : IterV[E,A] = {
      try {
	if (x ne null) 
	  intern(x.parser, x.i, x.bytes, x.depth, x.started)
	else
	  intern(parser, i, 
	       if (parser.buffer.hasArray)
		 () => jbytes(parser)
	       else {
		 // perfectly valid for a mem mapped to be huge, in which case, we would have grief ?
		 val d = direct(Array.ofDim[Byte](parser.buffer.capacity)) _
		 () => d(parser)
	       }, -1, false
	     )
      } catch {
	case e : ContMe[E,A] => Cont( ( _ : Input[E] ) =>
	  callInt(e) /*
	    re-use error handling and
	    when the data is empty in prolog or endmisc send back a continuation to process more, when in the root elem itself then the iteratee i should deal with it
	    */ 
	   )
      }
    }
    callInt( null.asInstanceOf[ContMe[E,A]] )
  }
}
