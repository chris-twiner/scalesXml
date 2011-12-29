package scales.xml

import org.xml.sax.InputSource
import javax.xml.parsers.SAXParser
import scales.utils._
import java.io._

trait XmlParserImplicits {
  implicit def streamToSource( source : InputStream ) = new org.xml.sax.InputSource(source)
  implicit def readerToSource( source : Reader ) = new org.xml.sax.InputSource(source)
  /**
   * will call openStream.
   */ 
  implicit def urlToSource( url : java.net.URL ) = new org.xml.sax.InputSource(url.openStream)
}

trait XmlParser {

  import ScalesXml.xmlCBF

  /**
   * Use a custom parserPool to control the sax factory features 
   */ 
  def loadXml[Token <: OptimisationToken](source: InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation, parsers : Loaner[SAXParser] = DefaultSAXParserFactoryPool.parsers)(implicit xmlVer : XmlVersion): Doc = {
    var handler = new Handler(strategy)
    parsers.loan {
      parser =>
      parser.setProperty("http://xml.org/sax/properties/lexical-handler", handler)
      parser.parse(source, handler)

      Doc(handler.buf.tree, handler.prolog, handler.end)
    }
  }
  

  /** Elems can have any QName, attribs only prefixed or default */
  def eqn[Token <: OptimisationToken](uri: String,
    localName: String,
    qName: String, strategy : MemoryOptimisationStrategy[Token], token : Token): QName =
    if (uri.length() == 0) {
      // can only be in qName
      strategy.noNamespaceQName(qName, token)
    } else {
      // might have a prefix
      if (qName == localName) {
        // no prefix
	strategy.unprefixedQName(localName, uri, token)
      } else {
        // break up the prefix
        val bits = qName.split(':')
	
	strategy.prefixedQName(localName, uri, bits(0), token)
      }
    }

  /** attribs only prefixed or default */
  def aqn[Token <: OptimisationToken](uri: String,
    localName: String,
    qName: String, strategy : MemoryOptimisationStrategy[Token], token : Token): AttributeQName =
    if (uri.length() == 0) {
      // can only be in qName
      strategy.noNamespaceQName(qName, token) // Right
    } else {
      // might have a prefix
      if (qName == localName) {
        // no prefix
        scales.utils.error("Should not have an attribute with a unprefixed namspace")
      } else {
        // break up the prefix
        val bits = qName.split(':')
        strategy.prefixedQName(localName, uri,bits(0), token) // Left
      }
    }

  class Handler[Token <: OptimisationToken](val strategy : PathOptimisationStrategy[Token])(implicit val defaultVersion : XmlVersion) extends org.xml.sax.ext.DefaultHandler2 {

    import scales.utils.{noPath, Path, top, ScalesUtils }
    import org.xml.sax._
    import scala.collection.immutable.Stack
    import ScalesUtils._
//    import ScalesXml.toQName // Note we aren't validating the names here anyway so we don't need to use the correct xml version, future version may double check perhaps?

    implicit val weAreInAParser : FromParser = IsFromParser

    val token : Token = strategy.createToken

    // start with nothing
//    var path: XmlPath = noXmlPath

    // only trees have kids, and we only need to keep the parent
    val buf = new TreeProxies()

    var isCData = false

    // declarations on this element
    var nsDeclarations = emptyNamespaces

    var prolog = Prolog()
    var end = EndMisc()

    // used for judging PI or Comments
    var inprolog = true

    override def startDTD(name : String, publicId : String, systemId : String) {
      prolog = prolog.copy(dtd = Some(DTD(name, publicId, systemId)))
    }

    override def processingInstruction(target : String, data : String) {
      val pi = PI(target, data)
      addMisc(Right(pi))
    } 

    override def startPrefixMapping(prefix: String, uri: String) {
      nsDeclarations += (prefix -> uri)
    }

    override def startElement(uri: String,
      localName: String,
      qName: String,
      attributes: org.xml.sax.Attributes) {

      inprolog = false

      var i = 0
      val length = attributes.getLength
      var attribs = emptyAttributes

      while (i < length) {
        val qname = aqn(attributes.getURI(i), attributes.getLocalName(i), attributes.getQName(i), strategy, token)
        attribs = attribs unsafePlus 
	    strategy.attribute(qname, 
			attributes.getValue(i), 
			token)

        i += 1
      }
      
      // use the current nsMap
      val elem = strategy.elem(
	  eqn(uri, localName, qName, strategy, token)
	     , attribs, nsDeclarations, token)

      // reset the map
      nsDeclarations = emptyNamespaces

      // give it a fake mutable default to work with
      strategy.beginSubTree(buf, elem, token)
    }

    override def endElement(uri: String,
      localName: String,
      qName: String) {

      // pop it and we are now with the correct parent
      // let the strategy decide what actually happens
      strategy.elementEnd(buf, token)
    }

    override def comment(ch: Array[Char], offset: Int, length: Int): Unit = {
      val text = new String(ch, offset, length)

      addMisc( Left(Comment(text)) )      
    }

    def addMisc( miscItem : Misc ) {
      if (inprolog) 
	prolog = prolog.copy(misc = prolog.misc :+ miscItem)
      else {
	// need to tell if its finished the root elem or not
	if (buf.depth == -1)
	  end = end.copy( misc = end.misc :+ miscItem)
	else
	  // add it like a normal child
	  buf.addChild( miscItem.fold[XmlItem](x=>x, y=>y)) 
      }
    }

    override def characters(ch: Array[Char], offset: Int, length: Int): Unit = {
      val text = new String(ch, offset, length)

      val child: XmlItem = if (isCData) CData(text) else Text(text)

      // append as child
      buf.addChild(child)
    }

    override def startCDATA() { isCData = true; }
    override def endCDATA() { isCData = false; }

  }

}

class TreeProxy( var elem : Elem, var children : XmlChildren)

import scala.collection.mutable.ArrayBuffer

/**
 * Mutable list that keeps the item creation to a minimum, no extra garbage here until the parse is done...
 */ 
class TreeProxies( var depth : Int = -1, var proxies : ArrayBuffer[TreeProxy] = ArrayBuffer[TreeProxy]() ){
  import ScalesXml.xmlCBF

  var size = proxies.length

  var current : TreeProxy = _

  def addChild( i : XmlItem ) {
    //println("proxies addChild "+depth)
    current.children = (current.children :+ i)
  }

  def elementEnd() {
    val l = current
    //println("proxies elementend "+depth)
    if (depth > 0) {
      depth -= 1
      current = proxies( depth )
      current.children = (current.children :+ Tree(l.elem, l.children))
    } else {
      depth -= 1
    }
  }

  def beginSub( elem : Elem, children : XmlChildren = emptyChildren ) {
    //println("proxies beginSub "+depth)
    depth += 1
    
    if (depth == size) {
      current = new TreeProxy(elem, children)
      proxies += (current)
      size +=1
    } else {
      current = proxies(depth)
      current.elem = elem 
      current.children = children
    }
  }

  /**
   * Only call when its the end of the parse
   */ 
  def tree = {
    val tp = proxies(0)

    Tree(tp.elem, tp.children)
  }
}
