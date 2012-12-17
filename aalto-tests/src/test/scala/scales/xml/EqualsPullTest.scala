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

  import impl.NoVersionXmlReaderFactoryPool

  import junit.framework.Assert._
  import java.io._
  import scales.utils._
  import ScalesUtils._

  import parser.strategies._

  import org.xml.sax.{InputSource, XMLReader}

  override def doLoadXml[Token <: OptimisationToken](in : InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionXmlReaderFactoryPool, strategy = strategy)
  }

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

    var pc1 = pull(c1)
    var pm2 = pull(m2)

    assertFalse("pm2 should not === pc1", pm2 === pc1)

    pc1 = pull(c1)
    pm2 = pull(m2)

    val countRes = compare(pm2, pc1)
    assertTrue("countRes should be defined", countRes.isDefined)

    val Some((DifferentNumberOfMiscs(cl,cr,true), ccontext)) = countRes
    assertTrue("cl eq misc2", cl === misc2)
    assertTrue("cr eq miscN", cr === miscN)

    // swap the end out
    val c2 = m2.copy(end = EndMisc(miscN))

    val pc2 = pull(c2)
    pm2 = pull(m2)
    
    val ncountRes = compare(pm2, pc2)
    assertTrue("ncountRes should be defined", ncountRes.isDefined)

    val Some((DifferentNumberOfMiscs(ncl,ncr,false), nccontext)) = ncountRes
    assertTrue("ncl eq misc1", ncl === misc1) // ends are different on purpose
    assertTrue("ncr eq miscN", ncr === miscN)
  }


}
