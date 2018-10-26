package scales.utils.io

import java.nio.ByteBuffer

import scales.utils.resources._

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
 * Pools byte arrays
 */ 
class ByteArrayPool( val byteArraySize: Int ) extends SimpleUnboundedPool[Array[Byte]]{

  def create = 
    Array.ofDim[Byte](byteArraySize)

}

/**
 * Default buffer pool backed by byte arrays
 */ 
object DefaultBufferPool extends JVMBufferPool {
}

/**
 * Default Byte Array pool with 8k chunks
 */ 
object DefaultByteArrayPool extends ByteArrayPool(8192) {
}
