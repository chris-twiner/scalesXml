package scales.xml.impl

import scales.xml.{EmptyNamespace, Namespace, Xml10, XmlVersion}

import scala.sys.error


trait DefaultXmlVersion {
  implicit val defaultVersion : XmlVersion = Xml10
}

object NamespaceDefaults {
  /**
   * placeholder for the current element default namespace
   */
  protected[xml] val namespace = Namespace("")(Xml10,IsFromParser)

  /**
   * The no namespace namespace (xmlns="")
   */
  val noNamespace = EmptyNamespace
}


/**
 * Follows the XML 1.0 spec and XML 1.1 spec, all underlying code provided by a copied version of the Xerces libs.
 *
 * Throughout user QName code we default to XML 1.0, as all XML 1.0 docs are valid XML 1.1's but the reverse is not true.
 *
 * Also note that we do not accept :, we are namespace compliant first and foremost
 *
 * The reason I'm simply copying to another package is that the utils.XMLChar functions aren't then forcing a JAXP or
 * xerces version.
 */
object QNameCharUtils {
  import scales.org.apache.xerces.util._

  def validLocalName( name : String )(implicit ver : XmlVersion) =
    validXmlName(name) && (
      if (name.length > 2) 
	name.substring(0,2).toLowerCase != "xml"
      else
	true
    ) &&
    name.indexOf(':') == -1 // namespaces ftw

  def validXmlName( name : String )(implicit ver : XmlVersion)  =
    if (ver eq Xml10)
      XMLChar.isValidName(name)
    else
      XML11Char.isXML11ValidName(name)

  def validXmlPrefix( prefix : String )(implicit ver : XmlVersion) = 
    validXmlName(prefix) &&// other xmlns etc validations must be done in the actual qnames and elements
    prefix.indexOf(':') == -1

  def validXmlNamespace( namespace : String )(implicit ver : XmlVersion) =
    if ((ver eq Xml10) && (namespace.trim.length == 0))
      false // can't do empties in 1.0, only 1.1
    else // TODO - what else should we validate here, parsing URI / IRI?
      true

  /**
   * Validates and returns or throws
   */
  def validateLocalName( validLocal : String ) ( implicit ver : XmlVersion) = 
    if (validLocalName(validLocal)) 
      validLocal
    else
      error("The local name '"+validLocal+"' is not valid for Xml "+ver.version)

}

