package scales.xml

class AaltoDslBuildersTest extends DslBuildersTest {
  import junit.framework.Assert._

  import org.xml.sax.InputSource
  import ScalesXml._

  import impl.NoVersionXmlReaderFactoryPool
  import parser.strategies._

  override def doLoadXml[Token <: OptimisationToken](in : InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionXmlReaderFactoryPool, strategy = strategy)
  }

  override def testSimpleConvert = {
    val orig = <root><child/></root>
    val converted = orig.asScales(parsers = NoVersionXmlReaderFactoryPool)
    assertEquals("root", localName(converted.rootElem))
  }

}
