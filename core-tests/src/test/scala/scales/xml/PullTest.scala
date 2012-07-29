package scales.xml

class PullTest extends junit.framework.TestCase {

  import scaley.funny._
  import ScalesXml._

  def testIterator = {
    val pull = null.asInstanceOf[Iterator[PullType]]//pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = null.asInstanceOf[List[QName]]// List("log"l,"logentry"l)

    val it = iterate(LogEntries, pull)
  }
}
