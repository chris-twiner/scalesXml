package scales.xml.parser.pull.aalto

import scales.xml._
import scales.utils._
import io._
import resources._
import com.fasterxml.aalto._
import AsyncXMLStreamReader.EVENT_INCOMPLETE

import javax.xml.stream.XMLStreamConstants.END_DOCUMENT
import scales.xml.parser.pull.PullUtils
import scalaz.EphemeralStream
import scalaz.iteratee.{Enumerator, Input, Iteratee}
import EphemeralStream.emptyEphemeralStream
import scalaz.iteratee.Input.{Element, Eof}
import scalaz.iteratee.Iteratee.iteratee
import scalaz.iteratee.StepT.{Cont, Done}
import scales.xml.parser.strategies.{MemoryOptimisationStrategy, OptimisationToken}

/**
 * An AynscParser, a DataChunk is fed in via nextInput which, in turn, returns an Input[EphmeralStream[PullType]] of events.
 * When the Input is El then the stream may be evaluated to get all available events, and will return empty when no more for that data chunk is available.
 * 
 * See nextInput for more info.
 */
abstract class AsyncParser(implicit xmlVersion : XmlVersion) extends CloseOnNeed with DocLike {

  protected type Token <: OptimisationToken

  protected val strategy : MemoryOptimisationStrategy[Token]

  protected val feeder : AsyncByteArrayFeeder

  protected val token : Token

  protected val parser : AsyncXMLStreamReader[AsyncByteArrayFeeder]

  /**
   * Closes the feeder and parser
   */ 
  protected def doClose = { 
    feeder.endOfInput
    parser.close
  }

  // to capture the miscs.
  private[this] var docImpl = EmptyDoc()

  protected def copyProlog(p : Prolog) { 
    docImpl = docImpl.copy( prolog = p )
  }

  protected def addPrologMisc(m : PullType) { 
    copyProlog( 
      prolog.copy(
	misc = prolog.misc :+ PullUtils.getMisc(m, "prolog") 
      )
    )
  }

  protected def addEndMisc(m : PullType) { 
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
  protected def pumpMisc() : Input[PullType] = {

    val (event, num, odepth, oprolog, _) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

    depth = odepth

    if (oprolog != prolog) {
      // doc start
      copyProlog( oprolog ) 
    }
    // FUCKING MISC MUST RETURN EMPTY FOR ALL - OPTION SHIT!!!
    if (num == END_DOCUMENT) {
      // EOF - let the iter deal 
      //println("closing")
      closeResource
      //Some(EOF[T])
      Eof[PullType]
    } else if (num == EVENT_INCOMPLETE) {
      //println("event incomplete ")
      Input.Empty[PullType]//None//pumpInMisc
    } else if (odepth == -1) {
      // still misc
      //println("didn't get out of misc , none??")
      
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

      Input.Empty[PullType]//None
    } else {
      //println("actually started with first event")
      started = true
      // pump actual first event - yay !!, next depth -1 is endmisc
      Element(event)
    }
  }

  protected def pump() : Input[PullType] = {
    if (feeder.needMoreInput) {
      //println("needed more but we still pumped")
    }

    // don't have to re-read, let it push what it has
    if (depth == -1) {
      //println("pumping misc")
      var r = pumpMisc()
      // keep going if there are more events to process and are still in misc
      while(!feeder.needMoreInput && r(el = E => false, empty = true, eof = false)) {
        r = pumpMisc()
      }
      r
    } else {
      // 2nd > events
      val (event, num, odepth, oprolog, _) = PullUtils.pumpEvent(parser, strategy, token, prolog, depth)(eventHandler)

      depth = odepth

      if (num == END_DOCUMENT) {
        // EOF - let the iter deal -- should not occur here though, only when depth == -1
        //println("closing in pump")
        closeResource
        //Some(EOF[T])
        Eof[PullType]
      } else if (num == EVENT_INCOMPLETE) {
        // let the iter attempt to deal
        //empties += 1
        //Some(IterV.Empty[T])
        Input.Empty[PullType]
      } else {
        // actual data present, odepth -1 is looked at for the next iter
        //Some(El(event))
        Element(event)
      }
    }
  }

  // keep going until we get needs more input
  protected def nextStream(): EphemeralStream[PullType] = {
    if (isClosed || feeder.needMoreInput) { // keep num around?
    //  println("got to nextStream and empty")
      emptyEphemeralStream
    } else {
	// push one more off
      val pumped = pump
      pumped(
	el = e => {
//	  println("got some "+e)
	  EphemeralStream.cons[PullType](e, nextStream())
	},
	empty = {
//	  println("pumped all we have")
	  emptyEphemeralStream
	},
	eof = {
//	  println("eof from pump but returned empty")
	  emptyEphemeralStream // next run will pick it up
	}
      ) // 
    }
  }
  /**
   * Given a DataChunk will return the next available data stream.
   * Returns EOF when the Parser is closed, Empty when more DataChunks are needed.
   * Returning El[EphemeralStream[PullType]] provides a stream which will lazily evalulate all available Xml Events from this DataChunk.
   *
   *
   * NOTEs (see Aalto-xml feeder.feedInput for details on this):
   * 1) All existing events from the last El should be consumed before calling nextInput again
   * 2) The DataChunk passed in should not be re-used until the stream returns empty (or Empty/EOF is returned).
   */ 
  def nextInput(d: DataChunk): Input[EphemeralStream[PullType]] = {
    //println("called nextInput")

    if (d.isEmpty) {
      //println("fed empty")
      Input.Empty[EphemeralStream[PullType]]
    } else 
      if (isClosed || d.isEOF) {
        if (d.isEOF) {
          //println("Data was eof")
          closeResource
        }
        Eof[EphemeralStream[PullType]]
      } else {
        if (!feeder.needMoreInput) {
          error("The stream from the previous call to nextInput was not evaluated ")
        }

        //println("pushing "+d)
        feeder.feedInput(d.array, d.offset, d.length)

        val res = nextStream()

        if (res.isEmpty) { // let the enumerator deal with it
          //println("empty after feeding")
          Input.Empty[EphemeralStream[PullType]]
        } else // it may have empty after the call
          Element[EphemeralStream[PullType]](res)

    }
  }

  /**
   * Provides a ResumableIter that converts DataChunks via a parser into a stream of PullTypes.  Returns Done when there are results from the pushed chunks.
   *
   * Calls AsyncParser parse
   */  
  def iteratee: ResumableIter[DataChunk, EphemeralStream[PullType]] =
    AsyncParser.parse(this)
  
}

object AsyncParser {

  /**
   * Provides a ResumableIter that converts DataChunks via a parser into a stream of PullTypes.  Returns Done when there are results from the pushed chunks.
   */
  def parse(parser: AsyncParser): ResumableIter[DataChunk, EphemeralStream[PullType]] = {

    def EOF: ResumableStep[DataChunk, EphemeralStream[PullType]] = {
      parser.closeResource

      //println("closing against EOF from parse")
      Done((emptyEphemeralStream,
        iteratee(Cont(
          error("Called the continuation on a closed parser")
        ))), Eof[DataChunk])
    }

//    def emptyness : ResumableStep[DataChunk, EphemeralStream[PullType]] =
  //    Done((emptyEphemeralStream, iteratee( Cont(step))), Input.Empty[DataChunk])

    def step(s: Input[DataChunk]): ResumableIter[DataChunk, EphemeralStream[PullType]] =
      iteratee(
        s(el = e => {
            //println("Did get a large chunk "+e)
            val r = parser.nextInput(e)
            r( el = es => {
            //println("got el with es " + es.isEmpty + " feeder " + parser.feeder.needMoreInput)
                Done((es,
                  iteratee(Cont(
                    step
                    ))), Input.Empty[DataChunk])
                },
                empty =
                  //println("empty from input")
                  //emptyness
                  Cont(step)
                ,
                eof = EOF
            )
          },
          empty = {
            //println("empty input")
            //emptyness
            //Done((EphemeralStream.empty, Cont(step)), IterV.Empty[DataChunk]) // nothing that can be done on empty
            Cont(step)
          },
          eof = EOF
        )
      )

    iteratee( Cont(step) )
  }


  /**
   * Creates a parser based on the input channel provided
   */
  def apply[TokenT <: OptimisationToken]( optimisationStrategy : MemoryOptimisationStrategy[TokenT] = defaultOptimisation, parsers : Pool[AsyncXMLInputFactory] = AsyncXMLInputFactoryPool )( implicit xmlVersion : XmlVersion ) : AsyncParser = new AsyncParser(){
    type Token = TokenT
    val strategy = optimisationStrategy
    
    import PullUtils.weAreInAParser
    val token = strategy.createToken

    val pf = parsers.grab

    val parser = pf.createAsyncForByteArray()
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
