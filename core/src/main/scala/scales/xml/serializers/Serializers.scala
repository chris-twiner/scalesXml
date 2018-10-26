package scales.xml.serializers

import java.io.Writer
import java.nio.charset.Charset

import scales.utils._
import scales.xml._

/**
 * Interface used for serializing the XML events, same for both stream and tree.
 *
 * The default implementation validates the output via LSSerializer against a given encoding.
 *
 * An alternative FastSerializer simply outputs strings and is in no way correct but at least faster then going to StreamWriter for incorrectness.
 *
 * Serializers should use qName.ncName for serialization purposes of both attributes and elements, the QName is provided to allow for validation of ouput.
 *
 * If a function returns Some it should signal the termination of the serializing.  Implementations are expected to respect this approach, its for the benefit of all developers.
 *
 * List[QName] is provided to help if path relevant information is needed for serialization.  For example if a use case requires that a particular path is filtered out.  Purely for performance reasons the list is in reverse order with the current QName at the top and root at the bottom.
 *
 */
trait Serializer {

  def item(item: XmlItem, path: List[QName]): Option[Throwable]

  def emptyElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable]
  def startElement(qName: QName, attributes: Traversable[Attribute], namespaces: Map[String, String], declareDefaultNS: Option[String], path: List[QName]): Option[Throwable]
  def endElement(qName: QName, path: List[QName]): Option[Throwable]

  def xmlDeclaration(encoding: Charset, version: XmlVersion): Option[Throwable]

}

case class SerializerData(out: Writer, version: XmlVersion = ScalesXml.defaultVersion, encoding: Charset = defaultCharset)

/**
 * Serializer factories are responsible for the life cycle of serializers and
 * their resources.
 *
 */
trait SerializerFactory {
  type ExactSerializer <: Serializer
  
  def apply[R](thunk: Serializer => R)(data: SerializerData): R
  def borrow(data : SerializerData) : ExactSerializer
  def giveBack(serializer : ExactSerializer) : Unit
}

object SerializerHelpers {
  /**
   * Outputs the xml document declaration
   */
  def xmlDecl(out: Writer, encoding: java.nio.charset.Charset, version: XmlVersion): Option[Throwable] = {
    out.append("<?xml version=\"" + version.version + "\" encoding=\"" + encoding.displayName + "\"?>")
    None
  }

  /**
   * Default serializer for items, usefull for when the contents aren't correct but you want to see them anyway
   */
  def item(out: Writer, item: XmlItem, path: List[QName]): Option[Throwable] = {
    val res = item match {
      case Text(value) => value
      case CData(value) => "<![CDATA[" + value + "]]>"
      case Comment(value) => "<!--" + value + "-->"
      case PI(target, value) => "<?" + target + " " + value + "?>"
    }
    out.append(res)
    None
  }

}

