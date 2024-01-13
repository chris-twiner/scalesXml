package scales.utils.io

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

/**
 * Represents the end of a stream, no more Bytes are available
 */ 
object EOFData extends DataChunk {
  val array = Array.empty[Byte]
  val offset = 0
  val length = -1

  override val isEOF = true
}

/**
 * Represents a data stream with no immediate data to return.
 */ 
object EmptyData extends DataChunk {
  val array = Array.empty[Byte]
  val offset = 0
  val length = 0

  override val isEmpty = true
}

/**
 * Represents the full array
 */ 
final case class FullChunk( array: Array[Byte] ) extends DataChunk {
  def offset = 0
  def length = array.length
}

/**
 * A section of a Byte array.  Due to Trampolining this cannot be re-used, as such a copy is made.  TODO Perhaps this should be optional
 */ 
//final case class Chunk( array: Array[Byte], offset: Int, length: Int) extends DataChunk

object Chunk{
  def apply( array: Array[Byte], offset: Int, length: Int): DataChunk =
    FullChunk{
      val c = Array.ofDim[Byte](length)
      Array.copy(array, offset, c, 0, length)
      c
    }
}

