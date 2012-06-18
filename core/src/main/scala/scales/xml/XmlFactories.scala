package scales.xml

import javax.xml.parsers._
import javax.xml.stream._
import javax.xml.validation._
import javax.xml.transform._

import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLReaderFactory

/**
 * Most of the JAXP, STAX and DOM apis don't mention much about
 * thread safety but the dominant xerces is completely unsafe with regards
 * to threads.  In fact only the 1.4.2 apis mention anything about thread safety
 * expectations.
 *
 * The process of getting a factory is very expensive and are not thread safe (you can't trust it to create a document in parallel).  See MUSE 270 for an example of why.
 *
 * As such we must use a pool for all handling.  A thread/piece of code
 * grabs a factory from the pool, creating if necessary.
 *
 * To aid the user the parsing code uses the Loaner interface (for SAX and DOM factories) and uses the Pool interface directly for Pull Parsers, as using them does not imply a given scope.
 *
 * By default no validating is performed
 */ 
trait XmlFactories {

  /**
   * Default SAXParser Factory
   */ 
  object DefaultSAXParserFactoryPool extends scales.utils.SimpleUnboundedPool[SAXParserFactory] { pool =>
    
    def create = {
      val parserFactory = SAXParserFactory.newInstance()
      parserFactory.setNamespaceAware(true)
      parserFactory.setFeature("http://xml.org/sax/features/namespaces", true)
      parserFactory.setValidating(false)
      parserFactory
    }

    val parsers = new scales.utils.Loaner[SAXParser] {
      def loan[X]( tThunk : SAXParser => X ) : X =
	pool.loan{ x => tThunk(x.newSAXParser) }
    }
  }

  /**
   * Default XMLReader Factory
   */ 
  object DefaultXMLReaderFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] { pool =>
    
    def create = 
      XMLReaderFactory.createXMLReader()
    
  }    

  /**
   * Default DOMFactory impl
   */ 
  object DefaultDOMFactoryPool extends scales.utils.SimpleUnboundedPool[DocumentBuilderFactory] { pool =>
    
    def create = {
      val dbFactory = DocumentBuilderFactory.newInstance()
      dbFactory.setNamespaceAware(true)
      dbFactory.setValidating(false)
      dbFactory
    }

    val parsers = new scales.utils.Loaner[DocumentBuilder] {
      def loan[X]( tThunk : DocumentBuilder => X ) : X =
	pool.loan{ x => tThunk(x.newDocumentBuilder) }
    }
  }

  /**
   * Default StaxInputFactory impl
   */ 
  object DefaultStaxInputFactoryPool extends scales.utils.SimpleUnboundedPool[XMLInputFactory] { pool =>
    
    val cdata = "http://java.sun.com/xml/stream/properties/report-cdata-event"

    def create = {
      val fac = XMLInputFactory.newInstance()
      if (fac.isPropertySupported(cdata)) {
	fac.setProperty(cdata, java.lang.Boolean.TRUE);
      }
      fac
    }
  }

  /**
   * Default XSD SchemaFactory impl
   */ 
  object DefaultXSDSchemaFactoryPool extends scales.utils.SimpleUnboundedPool[SchemaFactory] { pool =>
    
    def create = {
      val fac = SchemaFactory.newInstance(javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI)
      fac
    }
						   
  }

  def newSchema( source : Source, factory : scales.utils.Loaner[SchemaFactory] = DefaultXSDSchemaFactoryPool ) =
    factory.loan{ 
      f => 

      import javax.xml.transform.stream._
      import ScalesXml._

      if (!Versions.newSchemaShouldSerialize) 
	f.newSchema(source)
      else 
	if (source.isInstanceOf[ScalesSource]) {
	  val s = source.asInstanceOf[ScalesSource]
	  f.newSchema(asStreamSource((s.stream, s.documentLike)))
	} else f.newSchema(source)
 
    }

}

/**
 * Lazy val needed to trap impl, need pluggable (slf4j style) to swap out different
 * logic, defaulting to sun jaxp ?  For a future version, env property is enough for now.
 *
 */ 
object Versions {

  /**
   * If the transformer is Xalan then serialize before creating a source
   */ 
  lazy val traxSourceShouldSerialize = {
    val p = System.getProperty("scales.traxSourceShouldSerialize")
    if (p ne null)
      p.toBoolean
    else {

      import javax.xml.transform._

      val t = TransformerFactory.newInstance.newTransformer
      t.getClass.getName == "org.apache.xalan.transformer.TransformerIdentityImpl"
    }
  }

  /**
   * If newSchema is called should we serialize the source?  Sun JDK does trax but not validation for StAXSource :<
   * 
   * JDK 1.7 shares the StAX love though.
   *
   * If scales.newSchemaShouldSerialize is defined its value is used.
   *
   * Otherwise we examine the implemenation version of jaxp schemafactory, 1.5 and 1.6 need to serialize, 1.7 (and hopefully above ^_^) don't.
   */ 
  lazy val newSchemaShouldSerialize = {
    val p = System.getProperty("scales.newSchemaShouldSerialize")
    if (p ne null)
      p.toBoolean
    else {
      val fac = SchemaFactory.newInstance(javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI)
      val ver = fac.getClass.getPackage.getImplementationVersion
      val pre = ver.substring(0,3)

      if (pre == "1.5" || pre == "1.6") 
	true
      else
	false
    }
  }
}
