package scales.utils

import org.junit.Assert.assertTrue
import scales.utils.collection.array.{IAEmpty, IAThree, ImmutableArray}
import scales.utils.collection.ImmutableArrayProxyLikeThing

class ImmutableArrayTests extends junit.framework.TestCase {

  def testAppends(): Unit = {
    val r = IAEmpty[Int]() ++ Seq(1,2,3)
    assertTrue("should be IAThree", r == IAThree(1,2,3))
    val r2 = r ++ Seq(4,5,6)
    assertTrue("should be ImmutableArray", r2 == ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer), 0, 6))
  }

  def testSeqLikeAppends(): Unit = {
    val r = IAEmpty[Int]() ++ Seq(1,2,3)
    assertTrue("should be IAThree", r == IAThree(1,2,3))
    val r2 = r ++ Seq(4,5,6)
    assertTrue("should be ImmutableArray", r2 == ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer), 0, 6))
  }

  def testSeqLikeAppendsOfSeqLike(): Unit = {
    val r = IAEmpty[Int]() ++ Seq(1,2,3)
    assertTrue("should be IAThree", r == IAThree(1,2,3))
    val r2 = r ++ Seq(4,5,6)
    assertTrue("should be ImmutableArray", r2 == ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer), 0, 6))
  }

  def testImmutableArrayAppend(): Unit = {
    val r = ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer), 0, 6) ++ Seq(1,2,3)
    assertTrue("should be ImmutableArray", r == ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer, 1:Integer, 2:Integer, 3:Integer), 0, 9))
    val r2 = r ++ Seq(4,5,6)
    assertTrue("should be ImmutableArray", r2 == ImmutableArray[Int](Array[AnyRef](1: Integer,2: Integer,3: Integer,4: Integer,5: Integer,6: Integer, 1:Integer, 2:Integer, 3:Integer,4: Integer,5: Integer,6: Integer), 0, 12))
  }

}
