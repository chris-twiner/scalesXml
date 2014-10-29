package scales.utils.collection

import scala.collection.immutable.Stack

trait StackUtils {

  // it is expected that the top is the same, afterwards recurse (as available) until a difference is seen
  @scala.annotation.tailrec
  final def compareStack(p1: Stack[Int], p2: Stack[Int]): Int = {
    if (p1.isEmpty && p2.isEmpty == false) {
      1 // the position could be parent of same item, parents first 
    } else if (p1.isEmpty == false && p2.isEmpty) {
      -1 // the position could be parent of same item, parents first 
    } else if (p1.isEmpty && p2.isEmpty) {
      0 // no idea what this should be other than == 0 from normal comparisom
    } else {
      // get the tops, compare, if same pop and recurse
      val t1 = p1.top
      val t2 = p2.top
      if (t1 == t2) {
        compareStack(p1.pop, p2.pop)
      } else {
        if (t1 < t2)
          1
        else
          -1
      }
    }
  }

  def sameBase(test: Stack[Int], against: Stack[Int]): Boolean = {

    /**
     * Should go into a seperate util class along with compare
     */
    @scala.annotation.tailrec
    def iSameBase(test: Stack[Int], against: Stack[Int]): Boolean = {
      if (test.isEmpty && against.isEmpty == false) {
        true // if it was empty to start its still true, if not against is still below this stack
      } else if (test.isEmpty == false && against.isEmpty) {
        false // base has more nodes
      } else if (test.isEmpty && against.isEmpty) {
        true // the same depth, and in this function the same position
      } else {
        // get the tops, compare, if same pop and recurse
        val t1 = test.top
        val t2 = against.top
        if (t1 == t2) {
          iSameBase(test.pop, against.pop)
        } else {
          false // different
        }
      }
    }
    iSameBase(test, against)
  }

}
