package scales.utils.iteratee

import scales.utils._
  
import scalaz.iteratee.Enumerator

/**
 * Iterates for every Done from a given resumable iteratee
 */ 
final class ResumableIterIterator[E,A]( e : Enumerator[E])(init : ResumableIter[E,A]) extends Iterator[A] {
  import ScalesUtils._
  
  var cur = (init &= e).eval
  var isdone = isDone(cur)
  var r = extract(cur)

  def next = {
    val t = r
    cur = (extractCont(cur) &= e).eval
    isdone = isDone(cur)
    r = extract(cur)
    t.get // note we check for defined in hasNext
  }

  def hasNext = isdone && !isEOF(cur) && r.isDefined
}
