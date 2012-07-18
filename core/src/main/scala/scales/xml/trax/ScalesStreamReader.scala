package scales.xml.trax

import scales.xml._

import javax.xml.transform.stream._
import javax.xml.stream._

import javax.xml.namespace.{QName => JQName, NamespaceContext}


import ScalesXml._

/**
 * The trax support, the stream reader in particular
 * is aimed at allowing xlst transformations, a further
 * serialization option and conversion to other DOMs
 */

/**
 * Provides a stream reader interface for StAX.
 */
trait ScalesStreamReader extends XMLStreamReader {
  import XMLStreamConstants._

  protected val itr : Iterator[PullType]
  protected var ev : PullType = _
  protected var evType : Int = START_DOCUMENT

  // have we fired the starting document?
  protected var startedDoc = false

  /**
   * Take just the prolog and end misc
   */
  protected val docLike : DocLike

  lazy val prologItr = docLike.prolog.misc.iterator

  lazy val endItr = docLike.end.misc.iterator

  def setEv() {
//    println("called with ev " + ev )
    evType = ev.fold( _ match {
      case _ : Elem => START_ELEMENT
      case _ : Text => CHARACTERS
      case _ : CData => CDATA
      case _ : Comment => COMMENT
      case _ : PI => PROCESSING_INSTRUCTION
    } , _ => END_ELEMENT)
//    println("set evType " + evType)
  }

  var doEndDoc = false

  var shouldPop = false
  var nc : TNC = EmptyNamespaceContext

  /**
   * Only usable when its an element of course
   * We copy to an array for indexed access and keep
   * using ev for map access
   */
  protected var attribs : Array[Attribute] = _ 

  def close() {}

  def getAttributeCount() : Int = attribs.size
  def getAttributeLocalName( index : Int ) : String = attribs(index).local
  def getAttributeName( index : Int) : JQName = {
    val att : QName = attribs(index)
    att match {
      case l : NoNamespaceQName => new JQName(l.local)
      case u : UnprefixedQName => new JQName(u.namespace.uri, u.local)
      case p : PrefixedQName => new JQName(p.namespace.uri, p.local, p.prefix.get)
    }
  }

  def getAttributeNamespace( index : Int) : String = attribs(index).namespace.uri
  def getAttributePrefix( index : Int) : String = attribs(index).prefix.getOrElse(null : String)
  def getAttributeType( index : Int ) : String = null : String
  def getAttributeValue( index : Int ) : String = attribs(index).value
  def getAttributeValue(namespaceURI : String, localName : String) : String = 
    ev.left.get.asInstanceOf[Elem].attributes(Namespace(namespaceURI).apply(localName)).map(_.value).getOrElse(null : String)

  def getCharacterEncodingScheme() : String = "UTF-16"
  
  def getElementText() : String = {
    if(getEventType() != XMLStreamConstants.START_ELEMENT) {
      throw new XMLStreamException(
	"parser must be on START_ELEMENT to read next text", getLocation());
    }
    var eventType = next();
    var buf = new StringBuilder();
    while(eventType != XMLStreamConstants.END_ELEMENT ) {
      if(eventType == XMLStreamConstants.CHARACTERS
	 || eventType == XMLStreamConstants.CDATA
	 || eventType == XMLStreamConstants.SPACE
	 || eventType == XMLStreamConstants.ENTITY_REFERENCE) {
	buf.append(getText());
      } else if(eventType == XMLStreamConstants.PROCESSING_INSTRUCTION
		|| eventType == XMLStreamConstants.COMMENT) {
	// skipping
      } else if(eventType == XMLStreamConstants.END_DOCUMENT) {
	throw new XMLStreamException(
	  "unexpected end of document when reading element text content");
      } else if(eventType == XMLStreamConstants.START_ELEMENT) {
	throw new XMLStreamException(
	  "element text content may not contain START_ELEMENT", getLocation());
      } else {
	throw new XMLStreamException(
	  "Unexpected event type "+eventType, getLocation());
      }
      eventType = next();
    }
    buf.toString(); 
  }

  def getEncoding() : String = "UTF-16"
  def getEventType() : Int = evType
  /**
   * Called for both start element and end element
   */ 
  def elemName = 
    ev.fold(e => e.asInstanceOf[Elem].name,
	    ee => ee.name)

  def getLocalName() : String = elemName.local
  def getLocation() : Location = EmptyStreamLocation
  def getName() : JQName = 
    elemName match {
      case l : NoNamespaceQName => new JQName(l.local)
      case u : UnprefixedQName => new JQName(u.namespace.uri, u.local)
      case p : PrefixedQName => new JQName(p.namespace.uri, p.local, p.prefix.get)
    }
  
  def getNamespaceContext() : NamespaceContext = nc
  def getNamespaceCount() : Int = nc.ns.size
  def getNamespacePrefix(index : Int) : String = nc.ns(index)._1
  def getNamespaceURI() : String = elemName.namespace.uri
  def getNamespaceURI( index : Int ) : String = nc.ns(index)._2
  def getNamespaceURI( prefix : String) : String = nc.getNamespaceURI(prefix)

  def getPIData() : String = ev.left.get.asInstanceOf[PI].value
  def getPITarget() : String = ev.left.get.asInstanceOf[PI].target

  def getPrefix() : String = elemName.prefix.getOrElse("")//null : String)
  def getProperty(name : String) : Object = null
  def getText() : String = ev.left.get.asInstanceOf[XmlItem].value
  def getTextCharacters() : Array[Char] = getText.toArray
  def getTextCharacters(sourceStart : Int, target : Array[Char], targetStart : Int, length : Int) : Int = {
    if (targetStart < 0 || targetStart > getTextLength())
      throw new IndexOutOfBoundsException("TargetStart was out of bounds: "+targetStart+" textLength "+getTextLength())
    
    if (length < 0 || (targetStart + length > getTextLength())) 
      throw new IndexOutOfBoundsException("TargetStart and Length was out of bounds: "+targetStart+" length "+length+" textLength "+getTextLength())
    
    System.arraycopy(getTextCharacters(), sourceStart, target, targetStart, length)
    length
  }

  def getTextLength() : Int = getText().size
  def getTextStart() : Int = 0

  def getVersion() : String  = docLike.prolog.decl.version.version

  def hasName() : Boolean = evType == START_ELEMENT || evType == END_ELEMENT
  def hasNext() : Boolean = prologItr.hasNext || itr.hasNext || endItr.hasNext || doEndDoc
  def hasText() : Boolean = evType == CDATA || evType == COMMENT || evType == CHARACTERS

  def isAttributeSpecified(index : Int) : Boolean = false // can't specify this
  def isCharacters() : Boolean = evType == CHARACTERS
  def isEndElement() : Boolean = evType == END_ELEMENT
  def isStandalone() : Boolean = standaloneSet
  def isStartElement() : Boolean = evType == START_ELEMENT
  def isWhiteSpace() : Boolean = getText.trim.size == 0

  def doPop {
    if (shouldPop) {
      nc = nc.parent
      shouldPop = false
    }
  }

  def next() : Int = {
    if (!startedDoc) {
      startedDoc = true
      return START_DOCUMENT
    }

    if (doEndDoc) {
      doEndDoc = false
//      println("doc end") 
      return END_DOCUMENT
    }

    if (prologItr.hasNext) {
      ev = prologItr.next.fold(x=>x, y=>y)
    } else if (itr.hasNext) {
      ev = itr.next
    } else {
      ev = endItr.next.fold(x=>x, y=>y)
    }
    
    setEv

    import NamespaceContextFunctions._

    evType match {
      case START_ELEMENT => 
//	println(" se : "+ev)
	val el = ev.left.get.asInstanceOf[Elem]
	attribs = el.attributes.toArray
	
	doPop
	nc = newContext(nc, el)
      case END_ELEMENT =>
//	println(" ee : "+ ev)
	shouldPop = true
      case _ => 
//	println(" item "+ev)
	doPop
    }

    if (!itr.hasNext && !endItr.hasNext) {
      doEndDoc = true
    }

    evType
  }

  // tough one as well, Skips any white space (isWhiteSpace() returns true), COMMENT, or PROCESSING_INSTRUCTION, until a START_ELEMENT or END_ELEMENT is reached.
  def nextTag() : Int = {
    var eventType = next()
    while((eventType == XMLStreamConstants.CHARACTERS && isWhiteSpace()) // skip whitespace
	  || (eventType == XMLStreamConstants.CDATA && isWhiteSpace()) 
	  // skip whitespace
	  || eventType == XMLStreamConstants.SPACE
	  || eventType == XMLStreamConstants.PROCESSING_INSTRUCTION
	  || eventType == XMLStreamConstants.COMMENT
	) {
      eventType = next()
    }
    if (eventType != XMLStreamConstants.START_ELEMENT && eventType != XMLStreamConstants.END_ELEMENT) {
      throw new XMLStreamException("expected start or end tag", getLocation())
    }
    eventType
  }

  def require(tipe : Int, namespaceURI : String , localName : String) {
    if (evType != tipe)
      throw new XMLStreamException("Type does not match", getLocation())
    
    if (namespaceURI != null && namespaceURI != getNamespaceURI())
      throw new XMLStreamException("Namespace does not match", getLocation())

    if (localName != null && localName != getLocalName())
      throw new XMLStreamException("LocalName does not match", getLocation())
  }

  def standaloneSet() : Boolean = docLike.prolog.decl.standalone
}

