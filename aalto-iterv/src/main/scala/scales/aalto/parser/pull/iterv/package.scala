package scales.aalto.parser.pull

import scales.xml._
import scales.utils.io.DataChunk
import scalaz.EphemeralStream 
import scales.iterv.ResumableIter

/**
 * This package provides Iteratees based upon scales-aalto.
 */ 
package object iterv {

  case class AsyncOps(parser: AsyncParser){
    /**
     * Provides a ResumableIter that converts DataChunks via a parser into a strem of PullTypes.  Returns Done when there are results from the pushed chunks.
     *
     * Calls AsyncParserIterV parse
     */
    def iteratee: ResumableIter[DataChunk, EphemeralStream[PullType]] =
      AsyncParserIterV.parse(parser)
  }

  /**
   * Syntactic provider of .iteratee to AsyncParsers
   */ 
  implicit def toAsyncOps(parser: AsyncParser): AsyncOps = AsyncOps(parser)
  
}
