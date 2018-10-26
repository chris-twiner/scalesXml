package scales.xml.parser.sax

import java.io._

import javax.xml.parsers.SAXParser
import org.xml.sax.{InputSource, XMLReader}
import scales.utils.resources._
import scales.xml._
import scales.xml.impl._
import scales.xml.parser.strategies._

trait XmlParserImplicits {
  implicit def streamToSource( source : InputStream ) = new org.xml.sax.InputSource(source)
  implicit def readerToSource( source : Reader ) = new org.xml.sax.InputSource(source)
  /**
   * will call openStream.
   */ 
  implicit def urlToSource( url : java.net.URL ) = new org.xml.sax.InputSource(url.openStream)
}

/**
 * Whilst Saxon, Xerces and nu.validator support certain combinations, this is an abstraction over those needed by the readXml function
 */ 
trait SaxSupport {
  def setLexicalHandler[T <: OptimisationToken]( reader : XMLReader, handler : Handler[T] ) : Unit
  def getXmlVersion( reader : XMLReader ) : AnyRef
}

/**
 * Assumes lexical-handler and document-xml-version are supported
 */ 
trait DefaultSaxSupport extends SaxSupport {

  def setLexicalHandler[T <: OptimisationToken]( reader : XMLReader, handler : Handler[T] ) : Unit =
    reader.setProperty("http://xml.org/sax/properties/lexical-handler", handler)

  def getXmlVersion( reader : XMLReader ) : AnyRef =
    reader.getProperty("http://xml.org/sax/properties/document-xml-version")
  
}

trait XmlParser {

  /**
   * Use a custom parserPool to control the sax factory features 
   */ 
  def loadXml[Token <: OptimisationToken](source: InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation, parsers : Loaner[SAXParser] with SaxSupport= DefaultSAXParserFactoryPool.parsers)(implicit xmlVer : XmlVersion): Doc =
    parsers.loan {
      parser =>
	readXml(source, strategy, parser.getXMLReader(), parsers)
    }

  def readXml[Token <: OptimisationToken](source: InputSource, strategy : PathOptimisationStrategy[Token], reader : XMLReader, saxSupport : SaxSupport )(implicit xmlVer : XmlVersion): Doc = {
    var handler = new Handler[Token](strategy)
    saxSupport.setLexicalHandler[Token](reader, handler)
    
    reader.setContentHandler(handler)
    reader.parse(source)

    val docVersion = {
      val f = saxSupport.getXmlVersion(reader)

      if (f eq null) Xml10
      else {
	val v = f.toString
	if (v == "1.1") 
	  Xml11
	else
	  Xml10	  
      }
    }

    Doc(handler.getBuf.tree, handler.getProlog.copy( 
      decl = handler.getProlog.decl.copy(version = docVersion)),
	handler.getEnd)
  }   
  
  /**
   * Use a custom parserPool to control the sax factory features 
   */ 
  def loadXmlReader[Token <: OptimisationToken](source: InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation, parsers : Loaner[XMLReader] with SaxSupport = DefaultXMLReaderFactoryPool)(implicit xmlVer : XmlVersion): Doc =
    parsers.loan {
      parser =>
	readXml(source, strategy, parser, parsers)
    }

}
