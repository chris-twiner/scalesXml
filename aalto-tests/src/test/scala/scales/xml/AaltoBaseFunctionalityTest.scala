package scales.xml

import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLReaderFactory

import ScalesXml._

object NoVersionFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with DefaultSaxSupport {
  
  // doesn't support xml version retrieval
  override def getXmlVersion( reader : XMLReader ) : AnyRef =
    null

  def create = 
    XMLReaderFactory.createXMLReader()
   
}

class AaltoBaseFunctionalityTest extends BaseFunctionalityTest {

  override   def doLoadXml[Token <: OptimisationToken](in : java.net.URL, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionFactoryPool, strategy = strategy)
  }
}
