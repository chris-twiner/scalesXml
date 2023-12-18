package scales.xml.serializers

import scales.xml._

import scales.utils._

import java.nio.charset.Charset

object SimpleSerializerFactory extends SerializerFactory {
  type ExactSerializer = SimpleSerializer

  def apply[R](thunk: Serializer => R)(sdata: SerializerData): R =
    thunk(borrow(sdata))

  def borrow( sdata : SerializerData ) : ExactSerializer =
    new SimpleSerializer { val data = sdata }

  def giveBack( serializer : ExactSerializer ) {}
}

/**
 * Performs no validation, sorting of attributes, extra spaces on the end of empty element declarations and uses the short form for empty elements.
 *
 * Basically its good for non-pretty printing debugging only
 *
 */
trait SimpleSerializer extends Serializer {

  val data: SerializerData
  import data._

  def item(item: XmlItem, path: List[QName]): Option[Throwable] =
    SerializerHelpers.item(out, item, path)

  def doElem(qName: QName, attribs: Traversable[Attribute], ns: Map[String, String], declareDefaultNS: Option[String]) {

    out.append("<" + qName.qName)

    declareDefaultNS.foreach { dns =>
      out.append(" xmlns=\"" + dns + "\"")
    }

    ns.foreach { x =>
      out.append(" xmlns:" + x._1 + "=\"" + x._2 + "\"")
    }

    import ScalesXml._

    attribs.foreach { x =>
      out.append(" " + x.name.qName + "=\"" + x.value + "\"")
    }
  }

  def emptyElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable] = {
    doElem(qName, attributes, namespaces, declareDefaultNS)
    out.append("/>")
    None
  }

  def startElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable] = {
    doElem(qName, attributes, namespaces, declareDefaultNS)
    out.append(">")
    None
  }

  def endElement(qName: QName, path: List[QName]): Option[Throwable] = {
    out.append("</" + qName.qName + ">")
    None
  }

  def xmlDeclaration(encoding: Charset, version: XmlVersion): Option[Throwable] = SerializerHelpers.xmlDecl(out, encoding, version)
}
