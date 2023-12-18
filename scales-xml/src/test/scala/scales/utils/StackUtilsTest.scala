package scales.utils

class StackUtilsTest extends junit.framework.TestCase {

  import junit.framework.Assert._

  import scala.collection.immutable.Stack

  def testSameBase = {
    val base = Stack(0,1,2,3)
    assertTrue( "456 on end", sameBase( base, Stack( 0, 1,2,3,4,5,6 )) )
    assertFalse( "4 but longer" , sameBase( base, Stack( 0, 1,2,4,4,5,6 )) )
    assertFalse( "4 same length" , sameBase( base, Stack( 0, 1,2,4 )) )
    assertFalse( "shorter length" , sameBase( base, Stack( 0, 1,2 )) )    
  }
}
