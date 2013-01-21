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

  // trigger zeros by force every now and then...
  private[this] val zeros = Iterator.continually(rand.nextInt(5))
  
  private[this] var next0 = zeros.next
  private[this] var cur0 = 0
  private[this] var count0 = 0
 
  private[this] var closed = false
  
  def read( buf : java.nio.ByteBuffer ) : Int = {
    cur0 += 1
    if (cur0 > next0) {
      count0 += 1
      if (count0 > 10) { // feed 10 in
	next0 = zeros.next
	cur0 = 0
	count0 = 0
	println("********* zeroed 10 times")
      }
      0
    } else {
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
  }

  protected def closeUnderlying: Unit

  def close = { closed = true; closeUnderlying }
  def isOpen = !closed
}
