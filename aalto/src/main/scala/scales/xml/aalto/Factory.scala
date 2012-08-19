package scales.xml.aalto

import com.fasterxml.aalto._
import com.fasterxml.aalto.stax.InputFactoryImpl

import java.nio.ByteBuffer

import scales.xml._

/**
 * Default AsyncXMLInputFactory impl
 */ 
object AsyncXMLInputFactoryPool extends scales.utils.SimpleUnboundedPool[AsyncXMLInputFactory] { pool =>
  
  val cdata = "http://java.sun.com/xml/stream/properties/report-cdata-event"

  def create = {
    val fac = new InputFactoryImpl()
    if (fac.isPropertySupported(cdata)) {
      fac.setProperty(cdata, java.lang.Boolean.TRUE);
    }
    fac
  }
										   }

/**
 * ByteBuffer.allocate(bufferSize) backed pool
 * 
 * 8k default buffer size with a pool reduction of 30.
 */ 
class JVMBufferPool( val bufferSize : Int = 8192, override val reduceSize : Int = 30 ) extends scales.utils.SimpleUnboundedPool[ByteBuffer] {
  
  def create =
    ByteBuffer.allocate(bufferSize)

}

/**
 * ByteBuffer.allocateDirect(bufferSize) backed pool
 * 
 * 8k default buffer size with a pool reduction of 30
 */ 
class DirectBufferPool( val bufferSize : Int = 8192, override val reduceSize : Int = 30 ) extends scales.utils.SimpleUnboundedPool[ByteBuffer] {
  
  def create =
    ByteBuffer.allocateDirect(bufferSize)

}

/**
 * Default buffer pool backed by byte arrays
 */ 
object DefaultBufferPool extends JVMBufferPool {
}
