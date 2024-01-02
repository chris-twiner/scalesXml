package scales.utils.iteratee

import scalaz.Monad
import scales.utils._
import scalaz.iteratee.{EnumeratorT}

/**
 * Iterates for every Done from a given resumable iteratee

final class ResumableIterIterator[E,F[_],A]( e : EnumeratorT[E,F])(init : ResumableIter[E,F,A])(implicit F: Monad[F]) extends Function0[F[Iterator[A]]] {
  override def apply: F[Iterator[A]] = F.bind(init.value){ step =>
    val init = step
    F.point(new Iterator[A] {
    import ScalesUtils._

    var cur = (init &= e).eval
    var isdone = isDone(cur)
    var r = extract(cur)

    def next = {
      val t = r
      cur = (extractCont(cur) &= e).eval
      isdone = isDone(cur)
      r = extract(cur)
      F.bind(t)(o => F.point(o.get)) // note we check for defined in hasNext
    }

    def hasNext = isdone && !isEOF(cur) && r.isDefined
  })
}
 */