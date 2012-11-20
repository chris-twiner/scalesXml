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
    if (feeder.needMoreInput) {
      println("needed more but we still pumped")
    }

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
    //println("called nextInput")

    if (isClosed) {
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
   *
  val parse: (DataChunk, AsyncParser2) => (Input[EphemeralStream[PullType]], AsyncParser2) = (dc: DataChunk, parser: AsyncParser2) => {
    val r = parser.nextInput(dc)
    println(r)
    (r, parser)
  }*/

  /**
   * Pumps a DataChunk into a parser
   */ 
  def parse(parser: AsyncParser2): ResumableIter[DataChunk, EphemeralStream[PullType]] = {

    def EOF: ResumableIter[DataChunk, EphemeralStream[PullType]] = {
      parser.closeResource

      Done((EphemeralStream.empty, 
	  Cont(
	    error("Called the continuation on a closed parser")
	  )), IterV.EOF[DataChunk])
    }

    def emptyness : ResumableIter[DataChunk, EphemeralStream[PullType]] = Done((EphemeralStream.empty, Cont(step)), IterV.Empty[DataChunk])

    def step(s: Input[DataChunk]): ResumableIter[DataChunk, EphemeralStream[PullType]] = 
      s(el = e => {
	  val r = parser.nextInput(e)
	  r( el = es => {
		//println("got el")
	      Done((es,
		    Cont(
		      step
		      )), IterV.Empty[DataChunk])
	      },
	      empty = {
		//println("empty from done")
		emptyness//Cont(step)
	      },
	      eof = EOF
	  )
	},
	empty = {
	  //println("doneage on empty")
	  emptyness
	  //Done((EphemeralStream.empty, Cont(step)), IterV.Empty[DataChunk]) // nothing that can be done on empty
	},
	eof = EOF
      )

    Cont(step)
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
