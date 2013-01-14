package scales.utils.collection

/**
 * Only used for 2.8.1 FlatMapIterator
 */ 
class FlatMapIteratorDummy()

/**
 * Provides a more usable flatMap for 2.8.1
 */ 
trait FlatMapIterator[+A] extends Iterator[A] { self => 

  /**
   * Lifted from 2.9.Xs flatMap
   */ 
  def flatMap[B]( f : A => TraversableOnce[B] )(implicit ev : FlatMapIteratorDummy) : Iterator[B] = new FlatMapIterator[B] {
    import Iterator.empty
    private var cur: Iterator[B] = empty
    def hasNext: Boolean =
      cur.hasNext || self.hasNext && { cur = f(self.next).toIterator; hasNext }
    def next(): B = (if (hasNext) cur else empty).next()	
  }
}

trait FlatMapImplicits {

  implicit val flatMapIteratorDummy = new FlatMapIteratorDummy()

}
