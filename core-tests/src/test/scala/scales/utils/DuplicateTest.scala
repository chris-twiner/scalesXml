package scales.utils
import ScalesUtils._

import collection.DuplicateFilter

class DuplicateTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._
  
  def testSimple = {
    val list = List(1,2,2,3,3,3,4,5)
    val filtered = DuplicateFilter(list)
    
    val sb = new StringBuilder()
    filtered.addString(sb)
    assertEquals("12345",sb.toString)
  }

  def testSimpleStrings = {
    val list = List("a","b","b")
    val filtered = DuplicateFilter(list)
    
    val sb = new StringBuilder()
    filtered.addString(sb)
    assertEquals("ab",sb.toString)
  }

  case class StringHolder( str : String )

  import scalaz._
  import Scalaz._

  implicit val she = equal[StringHolder] { (a, b) => a == b }
  
  def testSimpleCases = {
    val b = StringHolder("b")
    val list = List(StringHolder("a"),b, b, b)
    val filtered = DuplicateFilter(list)
    
    val sb = new StringBuilder()
    filtered.map(_.str).addString(sb)
    assertEquals("ab",sb.toString)
  }

}
