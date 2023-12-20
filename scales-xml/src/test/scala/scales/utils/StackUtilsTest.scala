package scales.utils

class StackUtilsTest extends junit.framework.TestCase {

  import junit.framework.Assert._

  def testSameBase = {
    val base = List(0,1,2,3)
    assertTrue( "456 on end", sameBase( base, List( 0, 1,2,3,4,5,6 )) )
    assertFalse( "4 but longer" , sameBase( base, List( 0, 1,2,4,4,5,6 )) )
    assertFalse( "4 same length" , sameBase( base, List( 0, 1,2,4 )) )
    assertFalse( "shorter length" , sameBase( base, List( 0, 1,2 )) )
  }
}
