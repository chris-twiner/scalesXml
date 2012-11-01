package scales.xml.aalto

import com.fasterxml.aalto._
import AsyncXMLStreamReader.EVENT_INCOMPLETE
import javax.xml.stream.XMLStreamConstants.END_DOCUMENT
import scales.xml._
import scales.utils._

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

import scalaz._
import scalaz.IterV._

sealed trait PullTypeEvidence[T]

object PullTypeEvidence {
  implicit val pullOnly : PullTypeEvidence[PullType] = new PullTypeEvidence[PullType]{}
}

/**
 * Represents an asnych parser.  T is a dummy for the enumerator, only PullType is supported.
 * Callers MUST ensure that close is called in the case of an exception as well as the end of processing, failing to do so will cause unnecessary ByteBuffer creation. 
 */ 
abstract class AsyncParser[T]( val parsers : Pool[AsyncXMLInputFactory], val channel : ReadableByteChannel, private val bytePool : Pool[ByteBuffer] )(implicit xmlVersion : XmlVersion, ev : PullTypeEvidence[T] ) extends CloseOnNeed with DocLike {

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

  protected var depth = -1
  protected var started = false

  //private var empties = 0

  /**
   * The document element has been reached
   */ 
  def startedElementProcessing = started
  
  protected val incompOrEnd : PullType = Left(Text("I am incomplete or doc end"))

  protected val eventHandler = (x : Int) => {
    if (x == EVENT_INCOMPLETE || x == END_DOCUMENT)
      incompOrEnd
    else
      error("Got an unexpected event type " + x +" cannot proceed.") 
  }

  /**
   * Pushes through Misc items in either prolog or the epilog
   */
  protected def pumpMisc() : Option[Input[T]] = {

    val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

    depth = odepth

    if (oprolog != prolog) {
      // doc start
      copyProlog( oprolog ) 
    }
    
    if (num == END_DOCUMENT) {
      // EOF - let the iter deal 
      //println("parser incompletes depth 1 "+ empties)
      closeResource
      Some(EOF[T])
    } else if (num == EVENT_INCOMPLETE) {
      None//pumpInMisc
    } else if (odepth == -1) {
      // still misc
      
      if (event.isLeft && (event.left.get eq PullUtils.dtdDummy)) {
	copyProlog( prolog.copy(dtd = Some(
	  DTD("", "", "") // DTD has funnyness TODO find out what it looks like
        )))
      } else {
	if (!started)
	  addPrologMisc(event)
	else
	  addEndMisc(event)	      
      }

      None
    } else {
      started = true
      // pump actual first event - yay !!, next depth -1 is endmisc
      Some(El(event.asInstanceOf[T]))
    }
  }

  def pump() : Option[Input[T]] = {
    // don't have to re-read, let it push what it has
    if (depth == -1) {
      pumpMisc()
    } else {
      // 2nd > events
      val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

      depth = odepth

      if (num == END_DOCUMENT) {
	// EOF - let the iter deal -- should not occur here though, only when depth == -1
	//println("parser incompletes in events "+ empties)
	closeResource
	Some(EOF[T])
      } else if (num == EVENT_INCOMPLETE) {
	// let the iter attempt to deal
	//empties += 1
	Some(IterV.Empty[T])
      } else {
	// actual data present, odepth -1 is looked at for the next iter
	Some(El(event.asInstanceOf[T]))
      }
    }
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

final class AsyncParserEnumerator extends Enumerator[AsyncParser] {

  def jbytes[E](parser : AsyncParser[E]) : (Array[Byte], Int) = {
    parser.buffer.clear()
    val read = parser.channel.read(parser.buffer)
    (parser.buffer.array, read)
  }

  def direct[E](to : Array[Byte], parser : AsyncParser[E]) : (Array[Byte], Int) = {
    parser.buffer.clear()
    val read = parser.channel.read(parser.buffer)
    if (read != -1) {
      parser.buffer.get(to)
    }
    (to, read)
  }

  private[this] class ContMe[E,A]( val parser: AsyncParser[E], val i: IterV[E,A], val bytes : () => (Array[Byte], Int)) extends Exception {
    override def fillInStackTrace() = this
  }

  def apply[E,A](parser: AsyncParser[E], i: IterV[E,A]): IterV[E,A] = {

    @annotation.tailrec
    def intern[E,A](parser: AsyncParser[E], i: IterV[E,A], bytes : () => (Array[Byte], Int)): IterV[E,A] =       
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
		//println("parser incompletes in read  "+ empties)
		intern(parser, k(EOF[E]), bytes)
	      case 0 =>
		// if its done, we can bomb early, if not we shouldn't ping pong
		// but dump out early
		val res = k(IterV.Empty[E])
		if (isDone(res)) 
		  res
		else
		  throw new ContMe[E,A](parser, res, bytes)
		//intern(parser, k(IterV.Empty[E]), bytes, depth, started)
	      case _ =>
		// feed data in
		parser.feeder.feedInput(ar, 0, read)
		
		if (parser.feeder.needMoreInput) {
		  // we try again...? in the meantime this is death, requires a buffer ring due to feedInput's docs
		  throw new NeedsRingBuffer("Was pumped, but needs more to do anything, which requires us to add a ring")
		} else {
		  // we can pump
		  //println("enough")
		  val o = parser.pump()
		  o match {
		    case Some(in) => 
		      if (IterV.Empty.unapply[E](in)) {
			// don't ping pong for empty 
			// as we can only go back out anyway
			val res = k(in)
			if (isDone(res))
			  res
			else
			  throw new ContMe[E,A](parser, res, bytes)
		      }
			else
			  intern(parser, k(in), bytes)
		    case _ => 
		      throw new ContMe[E,A](parser, i, bytes)
		  }
		  
		}
	    }
	    
	  } else {
	    //println("enough")
	    val o = parser.pump()
	    o match {
	      case Some(in) => 
		if (IterV.Empty.unapply[E](in)) {
		  // don't ping pong for empty 
		  // as we can only go back out anyway
		  val res = k(in)
		  if (isDone(res))
		    res
		  else
		    throw new ContMe[E,A](parser, res, bytes)
		}
		  else
		    intern(parser, k(in), bytes)
	      case _ => 
		throw new ContMe[E,A](parser, i, bytes)
	    }
	  }
	}

    def callInt( x : ContMe[E,A] ) : IterV[E,A] = {
      try {
	if (x ne null) 
	  intern(x.parser, x.i, x.bytes)
	else
	  intern(parser, i, 
	       if (parser.buffer.hasArray)
		 () => jbytes(parser)
	       else {
		 // perfectly valid for a mem mapped to be huge, in which case, we would have grief ?
		 var ar = Array.ofDim[Byte](parser.buffer.capacity)
		 () => direct(ar, parser)
	       })
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

/*


let the func hide the mutability, resulting iteratee could be mapped to turn Array[Byte] into the triple.
enumManyToOne(PullType -> X)(parser ( (Array[Byte], start, len) ) => Seq[PullType] ) 

*/

/**
 * Represents a chunk of data to feed into an async parser.
 * The instance is deemed "owned" by the asnyc parser until it requires more input. 
 */ 
sealed trait DataChunk {
  def array: Array[Byte]
  def offset: Int
  def length: Int

  def isEOF: Boolean = false
  def isEmpty: Boolean = false
}

object EOFData extends DataChunk {
  val array = Array.empty[Byte]
  val offset = 0
  val length = -1

  override val isEOF = true
}

object EmptyData extends DataChunk {
  val array = Array.empty[Byte]
  val offset = 0
  val length = 0

  override val isEmpty = true
}

case class FullChunk( array: Array[Byte] ) extends DataChunk {
  def offset = 0
  def length = array.length
}

case class Chunk( array: Array[Byte], offset: Int, length: Int) extends DataChunk


/**
 * Represents an asnych parser.  T is a dummy for the enumerator, only PullType is supported.
 * Callers MUST ensure that close is called in the case of an exception as well as the end of processing, failing to do so will cause unnecessary ByteBuffer creation. 
 */ 
abstract class AsyncParser2(implicit xmlVersion : XmlVersion) extends CloseOnNeed with DocLike {

  type Token <: OptimisationToken

  val strategy : MemoryOptimisationStrategy[Token]

  val feeder : AsyncInputFeeder

  val token : Token

  val parser : AsyncXMLStreamReader

  /**
   * Closes the feeder and parser
   */ 
  protected def doClose = { 
    try{ 
      throw new Exception("I iz here")
    } catch {
      case t:Throwable => t.printStackTrace()
    }
    feeder.endOfInput
    parser.close
  }

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


  protected var depth = -1
  protected var started = false

  //private var empties = 0

  /**
   * The document element has been reached
   */ 
  def startedElementProcessing = started
  
  protected val incompOrEnd : PullType = Left(Text("I am incomplete or doc end"))

  protected val eventHandler = (x : Int) => {
    if (x == EVENT_INCOMPLETE || x == END_DOCUMENT)
      incompOrEnd
    else
      error("Got an unexpected event type " + x +" cannot proceed.") 
  }

  /**
   * Pushes through Misc items in either prolog or the epilog
   */
  protected def pumpMisc() : Option[PullType] = {

    val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

    depth = odepth

    if (oprolog != prolog) {
      // doc start
      copyProlog( oprolog ) 
    }
    
    if (num == END_DOCUMENT) {
      // EOF - let the iter deal 
      println("closing")
      closeResource
      //Some(EOF[T])
      None
    } else if (num == EVENT_INCOMPLETE) {
      None//pumpInMisc
    } else if (odepth == -1) {
      // still misc
      
      if (event.isLeft && (event.left.get eq PullUtils.dtdDummy)) {
	copyProlog( prolog.copy(dtd = Some(
	  DTD("", "", "") // DTD has funnyness TODO find out what it looks like
        )))
      } else {
	if (!started)
	  addPrologMisc(event)
	else
	  addEndMisc(event)	      
      }

      None
    } else {
      started = true
      // pump actual first event - yay !!, next depth -1 is endmisc
      Some(event)
    }
  }

  def pump() : Option[PullType] = {
    // don't have to re-read, let it push what it has
    if (depth == -1) {
      pumpMisc()
    } else {
      // 2nd > events
      val (event, num, odepth, oprolog) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

      depth = odepth

      if (num == END_DOCUMENT) {
	// EOF - let the iter deal -- should not occur here though, only when depth == -1
	println("closing in pump")
	closeResource
	//Some(EOF[T])
	None
      } else if (num == EVENT_INCOMPLETE) {
	// let the iter attempt to deal
	//empties += 1
	//Some(IterV.Empty[T])
	None
      } else {
	// actual data present, odepth -1 is looked at for the next iter
	//Some(El(event))
	Some(event)
      }
    }
  }

  // keep going until we get needs more input
  def nextStream(): EphemeralStream[PullType] =
    if (isClosed || feeder.needMoreInput) // keep num around?
      EphemeralStream.empty
    else {
	// push one more off
      pump.map{ p =>
	EphemeralStream.cons[PullType](p, nextStream())
	     }.
      getOrElse(nextStream()) // 
    }

  def nextInput(d: DataChunk): Input[EphemeralStream[PullType]] = {
    if (isClosed) {
      println("EOF'd")
      IterV.EOF[EphemeralStream[PullType]]      
    } else {
      feeder.feedInput(d.array, d.offset, d.length)

      if (feeder.needMoreInput) // let the enumerator deal with it
	IterV.Empty[EphemeralStream[PullType]]
      else // it may have empty after the call
	IterV.El[EphemeralStream[PullType]](nextStream())
    }
  }
  
}

object AsyncParser2 {
  /**
   * Function to use with enumToMany
   */
  val parse: (DataChunk, AsyncParser2) => (Input[EphemeralStream[PullType]], AsyncParser2) = (dc: DataChunk, parser: AsyncParser2) => {
    val r = parser.nextInput(dc)
    println(r)
    (r, parser)
  }

  /**
   * Creates a parser based on the input channel provided
   */
  def apply[TokenT <: OptimisationToken]( optimisationStrategy : MemoryOptimisationStrategy[TokenT] = defaultOptimisation, parsers : Pool[AsyncXMLInputFactory] = AsyncXMLInputFactoryPool )( implicit xmlVersion : XmlVersion ) : AsyncParser2 = new AsyncParser2(){
    type Token = TokenT
    val strategy = optimisationStrategy
    
    import PullUtils.weAreInAParser
    val token = strategy.createToken

    val pf = parsers.grab

    val parser = pf.createAsyncXMLStreamReader()
    val feeder = parser.getInputFeeder

    /**
     * also handle the parsers pool
     */ 
    override protected def doClose = { 
      super.doClose
      parsers.giveBack(pf)
    }

  }

}
