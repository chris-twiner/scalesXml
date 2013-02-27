package scales.utils.io

import scales.utils._

import resources._

import scalaz._
import Scalaz._
import scalaz.IterV._

import java.io._
import java.nio.channels._

import java.nio.ByteBuffer

sealed trait DataChunkEvidence[T]

object DataChunkEvidence {
  implicit val justDataChunk: DataChunkEvidence[DataChunk] = 
    new DataChunkEvidence[DataChunk]{}
}

/**
 * Wraps a ReadableByteChannel to provide DataChunks, optionally closes the channel (defaults to closing).
 *
 * @directBufferArrayPool is used when there is a direct ByteBuffer only.
 */ 
class ReadableByteChannelWrapper[T](val channel: ReadableByteChannel, private val closeChannel: Boolean = true, private val bytePool: Pool[ByteBuffer] = DefaultBufferPool, private val directBufferArrayPool: Pool[Array[Byte]] = DefaultByteArrayPool )(implicit ev: DataChunkEvidence[T]) extends CloseOnNeed {

  protected val buffer = bytePool.grab
  
  protected val to: Array[Byte] = 
    if (buffer.hasArray)
      null
    else
      directBufferArrayPool.grab
      
  protected def doClose = {
    if (!buffer.hasArray) {
      directBufferArrayPool.giveBack(to)
    }
    bytePool.giveBack(buffer)
    
    if (closeChannel) {
      channel.close()
    }
  }

  protected def jbytes() : DataChunk = {
    buffer.clear()
    val read = channel.read(buffer)
    read match {
      case -1 => {
	closeResource
	EOFData
      }
      case 0 => EmptyData
      case _ => Chunk(buffer.array, 0, read)
    }
  }

  var leftInBuffer = 0

  protected def direct() : DataChunk = {
    // limit is rem when its not read anything
    if (leftInBuffer > 0) {
      // println("remains "+leftInBuffer)
      val used = math.min(leftInBuffer, to.length)
      if (leftInBuffer >= used) {
	// still got some left to push
	leftInBuffer = leftInBuffer - used
      } else {
	leftInBuffer = 0
      }
      // if there is still data read it out in a chunk
      buffer.get(to, 0, used)
      Chunk(to, 0, used)
    } else {

      buffer.clear()
      
      val read = channel.read(buffer)
      // println("read this "+read)
      val rem = buffer.remaining
      read match {
	case -1 => 
	  closeResource
	EOFData
	
	case 0 => EmptyData
	
	case _ => 

	  val used = math.min(math.min(rem, read), to.length)
	  if (read > used) {
	    leftInBuffer = read - used
	  }

	  buffer.rewind()
	  // println("calling get with "+used)
	  try{
	    buffer.get(to, 0, used)
	    //println("read "+ new String(to, 0, used, "UTF-8"))
	  } catch {
	    case t: Throwable => 
	      // println("threw "+t.getMessage)
	      t.printStackTrace
	      throw t
	  }
	  Chunk(to, 0, used)
      }
    }
  }

  /**
   * Receives the next chunk from the underlying 
   */ 
  def nextChunk: DataChunk = 
    if (buffer.hasArray)
      jbytes()
    else
      direct()

}

trait ReadableByteChannelWrapperImplicits {

  implicit def toRBCWrapper(channel: ReadableByteChannel)(implicit ev: DataChunkEvidence[DataChunk]): ReadableByteChannelWrapper[DataChunk] = new ReadableByteChannelWrapper(channel)


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

}
