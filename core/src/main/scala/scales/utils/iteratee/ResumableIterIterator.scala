package scales.utils.iteratee

import scales.utils._
  
import scalaz.Enumerator

/**
 * Iterates for every Done from a given resumable iteratee
 */ 
final class ResumableIterIterator[E,A,F[_]]( it : F[E])(init : ResumableIter[E,A])(implicit e : Enumerator[F]) extends Iterator[A] {
  import ScalesUtils._
  
  var cur = init(it).eval
  var isdone = isDone(cur)
  var r = extract(cur)

  def next = {
    val t = r
    cur = extractCont(cur)(it).eval
    isdone = isDone(cur)
    r = extract(cur)
    t.get // note we check for defined in hasNext
  }

  def hasNext = isdone && !isEOF(cur) && r.isDefined
}
