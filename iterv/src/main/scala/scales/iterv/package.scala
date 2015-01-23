package scales

import scalaz.Enumerator
import scales.utils.io.DataChunker

package object iterv extends Iteratees with PullIteratees with SerializingIter {

  /**
   * Use in a call to asyncReadableByteChannelEnumerator to turn it into a synchronous enumerator (constantly trying to get new chunks of data)
   */
  val INFINITE_RETRIES = -1

  implicit def dataChunkerEnumerator[T[_] <: DataChunker[_]]: Enumerator[T] =
    new AsyncDataChunkerEnumerator[T]()

} 
