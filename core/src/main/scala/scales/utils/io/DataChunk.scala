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

final case class FullChunk( array: Array[Byte] ) extends DataChunk {
  def offset = 0
  def length = array.length
}

final case class Chunk( array: Array[Byte], offset: Int, length: Int) extends DataChunk

