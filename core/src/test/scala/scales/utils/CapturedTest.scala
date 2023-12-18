package scales.utils

class CaptureTest extends junit.framework.TestCase {

  import junit.framework.Assert._

  def testEmptyCapture : Unit = {
    val empty = List[Int]()
    val captured = capture(empty.iterator)
    assertFalse("Had a next",captured.hasNext)
  }

  def testFullCapture : Unit =
    doCaptureTest(5, captured => assertFalse("captured had a next "+getName(), captured.hasNext))

  def doCaptureTest( end : Int, partial : Iterator[Int] => Unit ) = {
    val some = List(1,2,3,4,5)
    val captured = capture(some.iterator)
    
    (1 to end).iterator.zip(captured).foreach(t => assertEquals(t._1,t._2))
    partial(captured)

    val replay = captured.restart
    (1 to 5).iterator.zip(replay).foreach(t => assertEquals(t._1,t._2))
    assertFalse("replay had a next "+getName(), replay.hasNext)

    try{
      captured.restart
    } catch {
      case t : Throwable => assertEquals(ALREADY_RESTARTED, t.getMessage())
    }
  }

  def testPartialCapture : Unit =
    doCaptureTest(3, captured => assertTrue("captured did not have a next "+getName(), captured.hasNext))
  
}
