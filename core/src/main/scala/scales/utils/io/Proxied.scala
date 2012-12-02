package scales.utils.io

import scales.utils.CloseOnNeed

/**
 * Wrap the Reader to allow better bracketing etc.
 */ 
case class ProxiedCloseOnNeedReader(orig : java.io.Reader) extends java.io.Reader with CloseOnNeed {
  require(orig != null, "original reader cannot be null")

  override def close() {}//NOOP
  override def mark(rah : Int) = orig.mark(rah)
  override def markSupported() = orig.markSupported()
  override def read() = orig.read()
  override def read(cbuf : Array[Char]) = orig.read(cbuf) 
  override def read(cbuf : Array[Char], off : Int, len : Int) = orig.read(cbuf,off,len) 
  override def read(target : java.nio.CharBuffer) = orig.read(target) 
  override def ready() = orig.ready 
  override def reset() = orig.reset 
  override def skip(n : Long) = orig.skip(n) 

  def doClose(){orig.close}
}

/**
 * Wrap the InputStream to allow better bracketing etc.
 */ 
case class ProxiedCloseOnNeedInputStream(orig : java.io.InputStream) extends java.io.InputStream with CloseOnNeed {
  require(orig != null, "original stream cannot be null")

  override def close() {}//NOOP
  override def available() = orig.available
  override def mark(rah : Int) = orig.mark(rah)
  override def markSupported() = orig.markSupported()
  override def read() = orig.read()
  override def read(cbuf : Array[Byte]) = orig.read(cbuf) 
  override def read(cbuf : Array[Byte], off : Int, len : Int) = orig.read(cbuf,off,len) 
  override def reset() = orig.reset 
  override def skip(n : Long) = orig.skip(n) 

  def doClose(){orig.close}
}
