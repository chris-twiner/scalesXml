package scales.utils.collection

trait IterableUtils {

  /** 
   * Makes a StringBuilder from an iterable using the builder parameter.
   */
  def mkString[A]( itr : Iterable[A], builder : java.lang.StringBuilder = new java.lang.StringBuilder(1000)
		  , separator : String = " ")( thunk : (A) => String) = {
    for( a <- itr ){
      builder.append(thunk(a))
      builder.append(separator)
    }
    // remove last 
    builder.delete(builder.length() - separator.length(), builder.length())
  }

  /**
   * Collects the first Some
   */ 
  def collectFirst[A, B]( in : Iterable[A] )( f : A => Option[B]) : Option[B] = {
    val it = in.iterator
    while( it.hasNext ) {
      val a = it.next
      val res = f(a)
      if (res.isDefined) {
	return res
      }
    }
    None
  }
  
  def capture[A](orig : Iterator[A] ) = new CapturedIterator(orig)

  val ALREADY_RESTARTED = "Iterator has already been restarted"

}

import scalaz.EphemeralStream
import scales.utils._

/**
 * Array backed buffer, restart returns the captured data and then rejoins the original iterator
 */ 
class CapturedIterator[A](orig : Iterator[A]) extends Iterator[A] {
  private[this] var buffer = new scala.collection.mutable.ArrayBuffer[A]()
  private[this] var haveRestarted = false
  def restart : Iterator[A] =
    if (haveRestarted) error(ALREADY_RESTARTED)
    else {
      haveRestarted = true
      buffer.iterator ++ orig
    }
  
  def hasNext = orig.hasNext
  def next = {
    val t = orig.next
    buffer += t
    t
  }
}


trait IterableUtilsImplicits extends FlatMapImplicits {

  /**
   * Lazy appenders for scalaz6.EphemeralStream
   */ 
  implicit def ephemeralAppender[A]( e : EphemeralStream[A] ) = new { 
    def append[A, B >: A]( a : EphemeralStream[A], e : => EphemeralStream[B] ) : EphemeralStream[B] = 
      if (!a.isEmpty) EphemeralStream.cons(a.headOption.get, append(a.tailOption.get, e))
      else e

    def +:+[B >: A]( e1 : => EphemeralStream[B]) : EphemeralStream[B] = append[A, B](e, e1)
  }

}
