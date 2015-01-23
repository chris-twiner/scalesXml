package scales.aalto.parser.pull.iterv

import scales.xml._
import scales.utils._
import io._
import resources._
import com.fasterxml.aalto._
import AsyncXMLStreamReader.EVENT_INCOMPLETE
import javax.xml.stream.XMLStreamConstants.END_DOCUMENT

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

import scales.xml.parser.pull.PullUtils
import scales.iterv.ResumableIter

import scalaz.{IterV, EphemeralStream, Input} 
import IterV._
import EphemeralStream.emptyEphemeralStream


import scales.xml.parser.strategies.{MemoryOptimisationStrategy, OptimisationToken}

import scales.aalto.parser.pull.AsyncParser


object AsyncParserIterV {

  /**
   * Provides a ResumableIter that converts DataChunks via a parser into a stream of PullTypes.  Returns Done when there are results from the pushed chunks.
   */
  def parse(parser: AsyncParser): ResumableIter[DataChunk, EphemeralStream[PullType]] = {

    def EOF: ResumableIter[DataChunk, EphemeralStream[PullType]] = {
      parser.closeResource

      //println("closing against EOF from parse")
      Done((emptyEphemeralStream, 
	  Cont(
	    error("Called the continuation on a closed parser")
	  )), IterV.EOF[DataChunk])
    }

    def emptyness : ResumableIter[DataChunk, EphemeralStream[PullType]] = Done((emptyEphemeralStream, Cont(step)), IterV.Empty[DataChunk])

    def step(s: Input[DataChunk]): ResumableIter[DataChunk, EphemeralStream[PullType]] = 
      s(el = e => {
	  //println("Did get a large chunk "+e)
	  val r = parser.nextInput(e)
	  r( el = es => {
		//println("got el with es " + es.isEmpty + " feeder " + parser.feeder.needMoreInput)
	      Done((es,
		    Cont(
		      step
		      )), IterV.Empty[DataChunk])
	      },
	      empty = {
		//println("empty from input")
		//emptyness
		Cont(step)
	      },
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

    Cont(step)
  }

}
