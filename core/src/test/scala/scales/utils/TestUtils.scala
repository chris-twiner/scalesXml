package scales.utils

object TestUtils {
  import junit.framework.Assert._

  /**
   * Should be an equals type class !!!!!
   * Provides comparisom of two different iterables with a conversion function, (both must support size being called)
   */
  def assertCompare[A, E](expectVals: Iterable[E], gotVals: Iterable[A])(convert: (A) => E) {
    if (expectVals.size != gotVals.size) println("got " + gotVals.map(convert))
    assertEquals(expectVals.size, gotVals.size)

    val vals = expectVals.zip(gotVals.map(convert(_)))
    for (aval <- vals) {
      assertTrue(aval.toString, aval._1 == aval._2)
    }
  }	

}
