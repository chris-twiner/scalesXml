package scales.utils.io

import java.io._
import java.nio.channels._

class RandomChannelStreamWrapper(val stream: java.io.InputStream, bufSize: Int) extends BaseRandomChannelWrapper(bufSize) {
  protected def fillBuffer(buffer: Array[Byte], len: Int): Int = 
    stream.read(buffer, 0, len)

  protected def closeUnderlying: Unit = stream.close
}

abstract class BaseRandomChannelWrapper(bufSize: Int) extends java.nio.channels.ReadableByteChannel {
  
  protected def fillBuffer(buffer: Array[Byte], len: Int): Int

  private[this] var _zeroed = 0
  
  def zeroed = _zeroed

  private[this] val ourbuf = Array.ofDim[Byte](bufSize)
  private[this] val rand = new scala.util.Random()

  private[this] var closed = false
  
  def read( buf : java.nio.ByteBuffer ) : Int = {
    val red = {
      val t = rand.nextInt(bufSize)
      if (t == 0) {
	_zeroed += 1
	0
      } else t
    }
    if (red != 0) {
      val did = fillBuffer(ourbuf, red)
      if (did > -1) {
	buf.put(ourbuf, 0, did)
      }
      did
    } else red
  }

  protected def closeUnderlying: Unit

  def close = { closed = true; closeUnderlying }
  def isOpen = !closed
}
