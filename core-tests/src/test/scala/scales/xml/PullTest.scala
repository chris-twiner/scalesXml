package scales.xml

class PullTest {

  import scaley.funny._

  def testIterator : Unit = {
    val pull = null.asInstanceOf[Iterator[PullType]]//pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = null.asInstanceOf[List[QName]]// List("log"l,"logentry"l)

    val it = iterate(LogEntries, pull)
  }
}

object CCE_Test extends App {
  new PullTest().testIterator
}
