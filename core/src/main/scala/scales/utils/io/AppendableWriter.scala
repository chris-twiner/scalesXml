package scales.utils.io

/**
 * To provide a writer to LSSerializer (and anything else that wants it ^_^)
 * we need to wrap appendables.
 */ 
case class AppendableWriter( out : Appendable ) extends java.io.Writer {
  def close() {}
  def flush() {}
  def write(cbuf : Array[Char], off : Int, len : Int) {
    var i = 0
    while(i < len){
      out.append(cbuf(off + i))
    }
  } 
}
