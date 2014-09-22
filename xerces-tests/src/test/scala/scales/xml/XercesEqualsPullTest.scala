package scales.xml.equalsTest

import scales.xml._
import ScalesXml._
import scales.utils._
import scales.xml.equals._
import junit.framework.Assert._
import collection.path._

/**
 * Pull specific tests in addition to the normal equals tests.
 * We only need to do one direct set of tests, the rest of the behavious is already tested
 */ 
class XercesEqualsPullTest extends EqualsNormalImportsTest {

  /**
   * https://issues.apache.org/jira/browse/XALANJ-1660 - interesting intrepretation of newline handling by xalan
   */ 
  override def testDifferenceAsStream : Unit = {
    val x = doLoadXml(scales.utils.resource(this, "/data/Nested.xml")).rootElem

    val y = x.fold_!( _.\*.\*("urn:default"::"ShouldRedeclare") )(_ => Remove())

    val Some((diff, context)) = compare[XmlTree](x, y)

    val upTo = context.toDifferenceAsStream(x)

    val upToStr = asString(upTo.iterator)
    val ln = System.getProperty("line.separator")
    val xalanConvertedCount = 
      if (ln != null && ln.size == 2 )
	upToStr.filter(_=='\n').size // xalan will swap the \n out with an extra \r
      else
	0
    assertEquals(232, upToStr.size - xalanConvertedCount)
  }

}
