package scales.xml

import ScalesXml._

class AaltoBaseFunctionalityTest extends test.BaseFunctionalityTest {

  import impl.NoVersionXmlReaderFactoryPool
  import parser.strategies._

  override   def doLoadXml[Token <: OptimisationToken](in : java.net.URL, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionXmlReaderFactoryPool, strategy = strategy)
  }
}
