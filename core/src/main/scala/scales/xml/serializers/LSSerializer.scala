package scales.xml.serializers

import java.io.Writer

import java.nio.charset.Charset

import scales.xml._
import impl._

import scales.utils._
import resources._

import javax.xml.parsers._

/**
 * Base implementation for a correct serializer using LSSerializer to provide escape character references.
 *
 * Developers can override this directly implementing encF, or choose to override createSerializer to further change serialization behaviour
 */ 
trait LSSerializerFactoryBase extends SerializerFactory {
  import org.w3c.dom._
  import ls._

  type ExactSerializer = serializers.LSSerializer

  /**
   * Override to create the serialazer
   */ 
  def createSerializer( sdata: SerializerData, dbf : DocumentBuilderFactory) : serializers.LSSerializer = {
    val db = dbf.newDocumentBuilder
    val ndoc = db.newDocument()
    ndoc.setXmlVersion(sdata.version.version)

    new serializers.LSSerializer {
      val docBuilderF = dbf
      val data = sdata
      val encMap = encF(sdata.encoding)
      lazy val doc = ndoc
      lazy val impl = doc.getImplementation().asInstanceOf[DOMImplementationLS]
      lazy val lsout = impl.createLSOutput()
      lazy val lsaout = impl.createLSOutput()
      lazy val lss = impl.createLSSerializer()
    }
  }

  /**
   * Can the content be encoded in a given charset
   */
  val encF: Charset => String => Option[Throwable]

  def borrow( sdata : SerializerData ) : ExactSerializer = {
    val dbf = DefaultDOMFactoryPool.grab

    val s = createSerializer(sdata, dbf)
    s.lsout.setEncoding(sdata.encoding.displayName)
    s.lsout.setCharacterStream(sdata.out)
    s.lsaout.setEncoding(sdata.encoding.displayName)

    val dc = s.lss.getDomConfig // got to be the lss, as the xalan impl is unrelated 
    //println("Configured with LSSerializer " + s.lss.getClass.getName)
    //val xs = new org.apache.xml.serializer.dom3.LSSerializerImpl
    // if the user wants to add a cdata then we shouldn't split it, but "throw"
    // NOTE that no implementation does this for ä and only for ]]>
    dc.setParameter("split-cdata-sections", false)
    dc.setParameter("well-formed", true)
    dc.setParameter("xml-declaration", false)

    s    
  }

  def giveBack( serializer : ExactSerializer ) {
    DefaultDOMFactoryPool.giveBack( serializer.docBuilderF )
  }

  def apply[R](thunk: Serializer => R)(sdata: SerializerData): R = {
    val s = borrow(sdata)
    try {
      thunk(s)
    } finally {
      giveBack(s)
    }
  }

}

/**
 * XHTML serialization extra touches
 */ 
object LSSerializerFactoryXHTML extends LSSerializerConcurrentCacheFactoryXHTML {
}

trait LSSerializerConcurrentCacheFactoryXHTML extends LSSerializerConcurrentCacheFactory {
  import org.w3c.dom._
  import ls._

  /**
   * Override to create the serialazer
   */ 
  override def createSerializer( sdata: SerializerData, dbf : DocumentBuilderFactory) : serializers.LSSerializer = {
    val db = dbf.newDocumentBuilder
    val ndoc = db.newDocument()
    ndoc.setXmlVersion(sdata.version.version)

    new serializers.XHTMLLSSerializer {
      val docBuilderF = dbf
      val data = sdata
      val encMap = encF(sdata.encoding)
      lazy val doc = ndoc
      lazy val impl = doc.getImplementation().asInstanceOf[DOMImplementationLS]
      lazy val lsout = impl.createLSOutput()
      lazy val lsaout = impl.createLSOutput()
      lazy val lss = impl.createLSSerializer()
    }
  }
}


/**
 * Default implmementation of serialization
 */ 
object LSSerializerFactory extends LSSerializerConcurrentCacheFactory {
}

trait LSSerializerConcurrentCacheFactory extends LSSerializerFactoryBase {
  import java.util.concurrent.ConcurrentHashMap
  import scales.utils.collection.Once

  /**
   * On the assumption that a given apps qname population is larger than that of its possible encodings.
   */
  val globalEncMap = new ConcurrentHashMap[String, ConcurrentHashMap[Charset, Once[Option[Throwable]]]]

  val encF = { encoding: Charset =>
    val encoder = encoding.newEncoder

    /**
     * Perhaps silly optimisation given a bug about it but worth trying
     *
     * If the charset contains utf8 then its able to support all unicode characters that utf8 can.  Wierdly I would also have expected utf8 to contain32 as it contains utf16 - must verify.
     */
    if (encoding.contains(defaultCharset)) { s: String => None }
    else { s: String =>

      calcOnce(encoding, valueOf(s, globalEncMap)(new ConcurrentHashMap[Charset, Once[Option[Throwable]]]())) {
        if (encoder.canEncode(s))
          None // it can do it
        else
          Some(InvalidCharacterInMarkup(s))
      }
    }
  }

}

object LSSerializerNoCacheFactory extends LSSerializerNoCacheFactoryT {
}

/**
 * This variety does not use a thread safe global cache, use when your data
 * is progressivly radically different for each run.
 *
 * If you don't like this scheme either simply implement LSSerializerFactoryBase with your own caching needs.
 */
trait LSSerializerNoCacheFactoryT extends LSSerializerFactoryBase {
  import java.util.HashMap

  val encF = { encoding: Charset =>
    val encoder = encoding.newEncoder
    val map = new HashMap[String, Option[Throwable]]

    /**
     * See the above explanation for this
     */
    if (encoding.contains(defaultCharset)) { s: String => None }
    else { s: String =>

      var r = map.get(s)
      if (r eq null) {
        r = if (encoder.canEncode(s))
          None // it can do it
        else
          Some(InvalidCharacterInMarkup(s))

        map.put(s, r)
      }

      r
    }
  }

}

/**
 * Adds an extra space after an empty element
 */ 
trait XHTMLLSSerializer extends LSSerializer {
  import data._

  override def emptyElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable] =
    doElem(qName, attributes, namespaces, declareDefaultNS) orElse {
      out.append(" />")
      None
    }
}

/**
 * Default serializer, correctness first.  Uses the LSSerializer present in each DOM L3 impl.  XmlItems are always serialized with the LS, elements and attributes are however for speed reasons, verified for encoding once per QName; the attribute values themselves are written via Text nodes and LS.
 */
trait LSSerializer extends Serializer {
  import javax.xml.parsers._
  import org.w3c.dom._
  import ls._
  import java.util.concurrent.ConcurrentHashMap

  val data: SerializerData
  import data._

  val docBuilderF: DocumentBuilderFactory
  val doc: Document
  val impl: DOMImplementationLS
  val lsout: LSOutput
  val lss: ls.LSSerializer

  val lsaout: LSOutput

  /**
   * Can the ncName be mapped for a given encoding
   */
  val encMap: String => Option[Throwable]

  lazy val textNode = doc.createTextNode("")

  lazy val encoder = encoding.newEncoder()

  /**
   * See default SF for logic on this, however faulty :-)
   */
  lazy val canEncode =
    if (encoding.contains(defaultCharset)) { s: String => true }
    else { s: String => encoder.canEncode(s) }

  def ct(t: => Boolean, s: => String): Option[Throwable] = {
    try {
      if (t)
        None
      else
        Some(CannotSerialize(s))
    } catch {
      case t: Throwable => Some(t)
    }
  }

  /**
   * Performs the actual write for Comments/CData/PI, due to LSSerializer issues this function is seperated for easy overriding, should anyone really trust their DOM/JAXP versions.
   *
   * For this method to be called the encoding has already been verified.
   */
  def writeNonText(item: XmlItem, path: List[QName]): Option[Throwable] =
    SerializerHelpers.item(out, item, path)

  /**
   * LSSerializer in 6_24 is garbage, but the xalan one isn't much better.  jre uses hex, xalan numberic character refs.  And jre is totally useless for cdata, writing no end part, both ignore the split-cdata-sections option when escaping, choosing to split, I assume this is only being done for character refs but follows the DOMConfiguration (instead of the load and save spec) for end sequence splitting.
   *
   * As such there only seems to be one safe way to handle this:
   *   # Use the encoder to check if a CData or Comment can be written without splitting, throw if it can't.
   *   # Write the start and end CData directly
   */
  def item(item: XmlItem, path: List[QName]): Option[Throwable] = {
    val canEncodeI: Option[Throwable] = item match {
      case x: scales.xml.Text =>
        None // never a problem
      case x: CData =>
        if (canEncode(item.value))
          None
        else
          Some(CDataCannotBeEncoded(item.value))
      case x: scales.xml.Comment =>
        if (canEncode(item.value))
          None
        else
          Some(CommentCannotBeEncoded(item.value))
      case x: scales.xml.PI =>
        if (canEncode(x.value) && canEncode(x.target))
          None
        else
          Some(PICannotBeEncoded("Target: " + x.target + " value: " + x.value))
    }
    canEncodeI orElse { // its serializeable, without cdata breaks

      item match {
        case x: scales.xml.Text =>
          textNode.setNodeValue(item.value) // twice for pi's but I'm not going to work around them
          ct(lss.write(textNode, lsout),
            {
              val str = new java.io.StringWriter();
              SerializerHelpers.item(str, item, path)
              str.toString
            })
        case _ =>
          writeNonText(item, path)
      }
    }
  }

  def writeAttr(before: => String, toCt: => String, after: => String) =
    ct({
      out.append(before)
      // every other escape takes place
      // 
      textNode.setNodeValue(toCt)
      val str = new java.io.StringWriter();
      lsaout.setCharacterStream(str)
      val r = lss.write(textNode, lsaout)
      lsaout.setCharacterStream(null) // let it be collected
      if (r) {
        out.append(str.toString.replaceAll("\"", "&quot;"))
        out.append(after)
      }
      r
    }, before + toCt + after)

  /**
   * Override this to order the attributes.
   */
  def doElem(qName: QName, attribs: Traversable[Attribute], ns: Map[String, String], declareDefaultNS: Option[String]): Option[Throwable] = {

    {
      if (qName.qNameVersion == Xml11 && version == Xml10)
        Some(IncompatibleQNameVersions(qName.qName))
      else
        None
    } orElse {
      // we don't have to encode it, just check it can be encoded, the writer will do the rest for markup
      encMap(qName.qName)
    } orElse {

      out.append("<" + qName.qName)
      None

    } orElse {

      declareDefaultNS.flatMap { dns =>
        writeAttr(" xmlns=\"", dns, "\"")
      }
    } orElse {

      ns.foldLeft(None: Option[Throwable]) { (r, x) =>
        r orElse {
          // check the prefix is valid
          if (QNameCharUtils.validXmlPrefix(x._1)(version) &&
	    QNameCharUtils.validXmlNamespace(x._2)(version)) {
            out.append(" xmlns:")
            encMap(x._1) orElse
              writeAttr(x._1 + "=\"", x._2, "\"")
          } else
            Some(IncompatibleQNameVersions("NS:"+x._1+"->"+x._2))
        }
      }

    } orElse {

      import ScalesXml._

      attribs.foldLeft(None: Option[Throwable]) { (r, x) =>
        r orElse {
	  val name = x.name
          // is the name valid
          if (name.qNameVersion == Xml11 && version == Xml10)
            Some(IncompatibleQNameVersions("Attr:"+name.qName))
          else {
            val n = name.qName
            encMap(n) orElse
              writeAttr(" " + n + "=\"", x.value, "\"")
          }
        }
      }

    }
  }

  def emptyElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable] =
    doElem(qName, attributes, namespaces, declareDefaultNS) orElse {
      out.append("/>")
      None
    }

  def startElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable] =
    doElem(qName, attributes, namespaces, declareDefaultNS) orElse {
      out.append(">")
      None
    }

  def endElement(qName: QName, path: List[QName]): Option[Throwable] = {
    out.append("</" + qName.qName + ">") // to get here it must be valid
    None
  }

  def xmlDeclaration(encoding: Charset, version: XmlVersion): Option[Throwable] = SerializerHelpers.xmlDecl(out, encoding, version)

}
