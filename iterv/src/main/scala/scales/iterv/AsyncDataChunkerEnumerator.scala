package scales.iterv

import scalaz.{IterV, Enumerator, Input}
import scalaz.IterV._

import scales.utils.io._

/**
 * Creates an Enumerator with a given count for Empty -> Cont applications.
 *
 * When the count is met it returns the Cont for the next Enumeration step.
 *
 * Note: Call via eval only.
 * @param contOnCont INFINITE_RETRIES (-1) for keep on trying, the default is 5 (as exposed by the implicit enumerator readableByteChannelEnumerator)
 */
class AsyncDataChunkerEnumerator[T[_] <: DataChunker[_]]( contOnCont: Int = 5 ) extends Enumerator[T] {
  def apply[E,A](chunker: T[E], i: IterV[E,A]): IterV[E, A] = {
    
    def apply(chunker: T[E], i: IterV[E,A], count: Int): IterV[E, A] = {
      i match {
	case _ if chunker.underlyingClosed || chunker.isClosed => i
	case Done(acc, input) => i
	case Cont(k) =>
	  val realChunk = chunker.nextChunk
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
	  apply(chunker, nextI, nc)
      }
    }

    apply(chunker, i, 0)
  }
}
