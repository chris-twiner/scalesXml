package scales.xml

/**
 * Test jaxp validation code via the TraxSupport
 */ 
class AaltoValidateTest extends ValidateTest {
  import org.xml.sax.InputSource
  import ScalesXml._

  import parser.strategies._
  import impl.NoVersionXmlReaderFactoryPool
 
  override def doLoadXml[Token <: OptimisationToken](in : InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionXmlReaderFactoryPool, strategy = strategy)
  }

}
