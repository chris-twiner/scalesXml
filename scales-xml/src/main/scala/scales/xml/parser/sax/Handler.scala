package scales.xml.parser.sax

import org.xml.sax.Locator
import org.xml.sax.ext.Locator2

import scales.xml._
import parser._
import scales.xml.parser.strategies.{PathOptimisationStrategy, OptimisationToken}

import scales.xml.impl.{NotFromParser, IsFromParser, FromParser}

import impl.{NameCreators, TreeProxies}

class Handler[Token <: OptimisationToken](strategy : PathOptimisationStrategy[Token])(implicit val defaultVersion : XmlVersion) extends org.xml.sax.ext.DefaultHandler2 {

  import org.xml.sax._

  import NameCreators._
  
  //    import ScalesXml.toQName // Note we aren't validating the names here anyway so we don't need to use the correct xml version, future version may double check perhaps?

  private[this] implicit val weAreInAParser : FromParser = IsFromParser

  private[this] val token : Token = strategy.createToken

  // only trees have kids, and we only need to keep the parent
  private[this] val buf = new TreeProxies()
  def getBuf = buf

  private[this] var isCData = false

  // declarations on this element
  private[this] var nsDeclarations = emptyNamespaces

  private[this] var prolog = Prolog()
  private[this] var end = EndMisc()

  def getProlog = prolog
  def getEnd = end

  private[this] var locator : Locator = _
  
  // used for judging PI or Comments
  private[this] var inprolog = true

  def checkit(what: String) {
  }

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

  override def setDocumentLocator( loc : Locator ) {
    locator = loc
  }

  override def startElement(uri: String,
			    localName: String,
			    qName: String,
			    attributes: org.xml.sax.Attributes) {

    if (inprolog) {
      if (locator.isInstanceOf[Locator2]) {
	val loc2 = locator.asInstanceOf[Locator2]
	prolog = prolog.copy( decl = prolog.decl.copy(encoding = (
	  if (loc2.getEncoding ne null) {
	    java.nio.charset.Charset.forName(loc2.getEncoding)
	  } else scales.utils.defaultCharset
	)))	  
      }
    }

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
