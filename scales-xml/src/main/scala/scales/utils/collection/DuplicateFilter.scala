package scales.utils.collection

import scalaz._
import Scalaz._

/**
 * Remove neighbouring duplicates based on a given predicate (defaults to _ eq _).
 * 
 * For example (1,2,2,3) would be converted to (1,2,3)
 * 
 * @author Chris
 *
 */
case class DuplicateFilter[T](orig: Iterable[T])(
  implicit predicate: Equal[T]) extends Iterable[T] {

  /**
   * Provide iterator to the path of the children
   */
  def iterator() = new Iterator[T]() {
    val itr = orig.iterator
    var nextPair: (Boolean, T) = getNext
    var isFirst = true
    def getNext: (Boolean, T) =
      if (itr.hasNext)
        (true, itr.next)
      else
        null: (Boolean, T)

    /**
     * 1, 2, 2, 3
     * 
     * 				nextPair	cur
     * construct		1
     * hasNext			1		-
     * hasNext.getNext	2		1
     * 	- true // pred == false
     * hasNext			2		-
     * hasNext.getNext	2		2
     * 	// pred == false
     * hasNext			2		-
     * hasNext.getNext	3		2
     * 	- true // pred == false
     * hasNext			3		-
     * hasNext.getNext	null	3
     * 	- false // nextPair eq null
     * 
     */
    def hasNext = if (isFirst) {
      //error("oops I iz testing init")
      isFirst = false
      (nextPair ne null)
    } else {
      if (nextPair ne null) {
        val cur = nextPair
        nextPair = getNext
        if (nextPair ne null) {
          // are they different?
          if (predicate.equal(cur._2, nextPair._2)) {
            // get again, recurse
            hasNext
          } else {
            true
          }
        } else {
          false
        }
      } else false
    }

    def next(): T =
      if (nextPair ne null) {
        nextPair._2
      } else throw new NoSuchElementException("next on empty iterator")

  }
}
