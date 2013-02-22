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
 * Wraps a ReadableByteChannel to provide DataChunks, optionally closes the channel (defaults to closing)
 */ 
class ReadableByteChannelWrapper[T](val channel: ReadableByteChannel, private val closeChannel: Boolean = true, private val bytePool: Pool[ByteBuffer] = DefaultBufferPool)(implicit ev: DataChunkEvidence[T]) extends CloseOnNeed {

  val buffer = bytePool.grab

  protected def doClose = {
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

  protected def direct(to : Array[Byte]) : DataChunk = {
    buffer.clear()
    val read = channel.read(buffer)
    read match {
      case -1 => {
	closeResource
	EOFData
      }
      case 0 => EmptyData
      case _ => 
	buffer.get(to)
      Chunk(to, 0, read)
    }
  }

  protected val bytes: () => DataChunk =
    if (buffer.hasArray)
      () => jbytes()
    else {
      // perfectly valid for a mem mapped to be huge, in which case, we would have grief ?
      var ar = Array.ofDim[Byte](buffer.capacity)
      () => direct(ar)
    }

  def nextChunk: DataChunk = bytes()

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
