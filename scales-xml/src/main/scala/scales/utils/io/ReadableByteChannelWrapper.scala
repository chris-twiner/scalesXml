package scales.utils.io

import scales.utils._
import resources._
import scalaz.{Id, Monad}
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.Iteratee.{enumEofT, enumOne, iteratee, iterateeT}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{Enumerator, EnumeratorT, Input, Iteratee, IterateeT, StepT}
import scales.utils.iteratee.functions.{ResumableIter, isDoneS, isEOFS}
import scales.utils.iteratee.monadHelpers.{CanRunIt, Performer}

import java.io._
import java.nio.channels._
import java.nio.ByteBuffer

sealed trait DataChunkEvidence[T]

object DataChunkEvidence {
  implicit val justDataChunk: DataChunkEvidence[DataChunk] = 
    new DataChunkEvidence[DataChunk]{}
}

object ReadableByteChannelWrapper {
  val emptyBytes = Array.ofDim[ Byte ](0)
}

/**
 * An abstraction over a stream that provides DataChunks
 */
trait DataChunker[T] extends CloseOnNeed {

  /**
   * Is the underlying resource closed - if true the dataChunkEnumerator will
   * return the Iteratee passed to it, stopping evaluation
   */ 
  def underlyingClosed = false

  protected implicit val ev: DataChunkEvidence[T]

  /**
   * Receives the next chunk from the underlying 
   */ 
  def nextChunk: DataChunk
}


/**
 * Wraps a ReadableByteChannel to provide DataChunks, optionally closes the channel (defaults to closing).
 *
 * This base implementation should only be used against already provided buffers, prefer using ReadableByteChannelWrapper directly instead.
 *
 * @constructor Direct buffers also require a backing array to be specified.
 */ 
class ReadableByteChannelWrapperBase[T](val channel: ReadableByteChannel, protected val buffer: ByteBuffer, val closeChannel: Boolean = true, protected val backingArray: Array[Byte] = ReadableByteChannelWrapper.emptyBytes )(implicit val ev: DataChunkEvidence[T]) extends DataChunker[T] with CloseOnNeed {

  if (!buffer.hasArray) {
    require(
      backingArray.length > 0,
      "A ReadableByteChannelWrapper with a Direct buffer must be created with a non empty backingArray")
  }

  override def underlyingClosed = !channel.isOpen
      
  /**
   * Closes the channel when closeChannel is true - ensure to call from derived classes
   */ 
  protected def doClose = {
    if (closeChannel) {
      channel.close()
    }
  }

  /**
   * Called when processing array backed buffers
   */ 
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

  private var leftInBuffer = 0
  
  /**
   * Called when processing via Direct Buffers
   */ 
  protected def direct() : DataChunk = {
    // limit is rem when its not read anything
    if (leftInBuffer > 0) {
      // println("remains "+leftInBuffer)
      val used = math.min(leftInBuffer, backingArray.length)
      if (leftInBuffer >= used) {
        // still got some left to push
        leftInBuffer = leftInBuffer - used
      } else {
        leftInBuffer = 0
      }
      // if there is still data read it out in a chunk
      buffer.get(backingArray, 0, used)
      Chunk(backingArray, 0, used)
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

          val used = math.min(math.min(rem, read), backingArray.length)
          if (read > used) {
            leftInBuffer = read - used
          }

          buffer.rewind()
          // println("calling get with "+used)
          //try{
            buffer.get(backingArray, 0, used)
            //println("read "+ new String(to, 0, used, "UTF-8"))
          //} catch {
          //  case t: Throwable =>
              // println("threw "+t.getMessage)
          //    t.printStackTrace
          //    throw t
          //}
          Chunk(backingArray, 0, used)
      }
    }
  }

  /**
   * Receives the next chunk from the underlying 
   */ 
  final def nextChunk: DataChunk = 
    if (buffer.hasArray)
      jbytes()
    else
      direct()

}

/**
 * Wraps a ReadableByteChannel to provide DataChunks, optionally closes the channel (defaults to closing).
 *
 * Can work with either direct or heap based buffers and uses pools to re-use the allocated buffers.
 * 
 * @param directBufferArrayPool is used when there is a direct ByteBuffer only.
 */ 
class ReadableByteChannelWrapper[T](channel: ReadableByteChannel, closeChannel: Boolean = true, private val bytePool: Pool[ByteBuffer] = DefaultBufferPool, private val directBufferArrayPool: Pool[Array[Byte]] = DefaultByteArrayPool )(implicit ev: DataChunkEvidence[T]) extends {

  override protected val buffer: ByteBuffer = bytePool.grab

  override protected val backingArray: Array[Byte] =
    if (buffer.hasArray)
      ReadableByteChannelWrapper.emptyBytes
    else
      directBufferArrayPool.grab

} with 
  ReadableByteChannelWrapperBase[T](
    channel, buffer, closeChannel, backingArray ) {
   
  override protected def doClose = {
    if (!buffer.hasArray) {
      directBufferArrayPool.giveBack(backingArray)
    }
    bytePool.giveBack(buffer)
    super.doClose
  }

}

/**
 * Convenience function for wrapping a channel
 */ 
class RBCImplicitWrapper(channel: ReadableByteChannel)(implicit ev: DataChunkEvidence[DataChunk]){
  def wrapped: DataChunker[DataChunk] = new ReadableByteChannelWrapper(channel)
}

trait ReadableByteChannelWrapperImplicits {

  implicit def toRBCWrapper(channel: ReadableByteChannel)(implicit ev: DataChunkEvidence[DataChunk]): RBCImplicitWrapper = new RBCImplicitWrapper(channel)

  implicit def dataChunkerEnumerator[T, F[_]: Monad](chunker: DataChunker[T]): EnumeratorT[DataChunk, F] =
    new AsyncDataChunkerEnumerator[T, F](chunker)

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
  class AsyncDataChunkerEnumerator[T, F[_]]( chunker: DataChunker[T], contOnCont: Int = 5 )(implicit F: Monad[F]) extends EnumeratorT[DataChunk, F] {
    import scalaz.Scalaz._

    override def apply[A]: StepT[DataChunk, F, A] => IterateeT[DataChunk, F, A] = {

      def apply(count: Int): StepT[DataChunk, F, A] => IterateeT[DataChunk, F, A] = {
        case i if chunker.underlyingClosed || chunker.isClosed =>
          i.pointI
        case i@Done(acc, input) =>
          val c = acc
          val in = input
          i.pointI
        case i =>
          i mapCont { k =>
            val realChunk = chunker.nextChunk
            iterateeT {
              val nc =
                if (realChunk.isEmpty)
                  count + 1
                else
                  0

              val shouldPause =
                if (((contOnCont != INFINITE_RETRIES) && (nc > contOnCont)))
                  true
                else
                  false

              val nextIStep =
                if (realChunk.isEOF)
                  k(Eof[DataChunk])
                else if (realChunk.isEmpty) {
                  if (shouldPause)
                    k(Element(SuspendData))
                  else
                    k(Empty[DataChunk])
                } else
                  k(Element(realChunk))

              F.bind(nextIStep.value) { step =>

                if (shouldPause && !isDoneS(step))
                  //nextIStep.value actually calls twice as it's then bound twice
                  F.point(step)
                else
                  apply(nc)(step).value

              }
            }
          }
      }

      apply(0)
    }
  }

}
