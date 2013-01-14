package scales.utils.io

import scales.utils.resources._
import java.nio.ByteBuffer

/**
 * ByteBuffer.allocate(bufferSize) backed pool
 * 
 * 8k default buffer size with a pool reduction of 30.
 */ 
class JVMBufferPool( val bufferSize : Int = 8192, override val reduceSize : Int = 30 ) extends SimpleUnboundedPool[ByteBuffer] {
  
  def create =
    ByteBuffer.allocate(bufferSize)

}

/**
 * ByteBuffer.allocateDirect(bufferSize) backed pool
 * 
 * 8k default buffer size with a pool reduction of 30
 */ 
class DirectBufferPool( val bufferSize : Int = 8192, override val reduceSize : Int = 30 ) extends SimpleUnboundedPool[ByteBuffer] {
  
  def create =
    ByteBuffer.allocateDirect(bufferSize)

}

/**
 * Default buffer pool backed by byte arrays
 */ 
object DefaultBufferPool extends JVMBufferPool {
}
