package scales.xml.equalsTest

import scales.xml._
import ScalesXml._

import scales.xml.equals._

/**
 * Pull specific tests in addition to the normal equals tests.
 * We only need to do one direct set of tests, the rest of the behavious is already tested
 */ 
class EqualsPullTest extends EqualsNormalImportsTest {
  // these guys import a resource
  import scalaz._
  import Scalaz._

  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._

  def pull( doc : Doc ) =
    pullXml(new StringReader(asString(doc)))

  def testPullDocHandling : Unit = {
    // we have to pull everything each time we want to compare
    var pd1 = pull(d1)
    var pd2 = pull(d2)
    
    assertFalse("pd1 shouldn't === pd2", pd1 === pd2)

    pd1 = pull(d1)
    var pd1_2 = pull(d1_2)
    
    assertTrue("pd1 should === pd1_2", pd1 === pd1_2)

    var pm1 = pull(m1)
    var pm2 = pull(m2)
    
    assertTrue("pm1 should === pm2", pm1 === pm2)
  }

  def testPullDifferentNumberOfMiscs : Unit = {
    // swap the first out
    val c1 = m2.copy(prolog = m2.prolog.copy(misc = miscN))

    assertFalse("m2 should not === c1", m2 === c1)

    val countRes = compare(m2, c1)
    assertTrue("countRes should be defined", countRes.isDefined)

    val Some((DifferentNumberOfMiscs(cl,cr,true), ccontext)) = countRes
    assertTrue("cl eq misc2", cl eq misc2)
    assertTrue("cr eq miscN", cr eq miscN)

    // swap the end out
    val c2 = m2.copy(end = EndMisc(miscN))
    
    val ncountRes = compare(m2, c2)
    assertTrue("ncountRes should be defined", ncountRes.isDefined)

    val Some((DifferentNumberOfMiscs(ncl,ncr,false), nccontext)) = ncountRes
    assertTrue("ncl eq misc1", ncl eq misc1) // ends are different on purpose
    assertTrue("ncr eq miscN", ncr eq miscN)
  }


}
