package scales.xml

import scales.utils.error
import scales.utils.{LeftLike, RightLike}

/**
 * Scales supports many aspects of Xml10 and Xml11, verification of serialization and values takes place with the XmlVersion ADT. 
 */ 
sealed trait XmlVersion{
  val version : String
}
case object Xml11 extends XmlVersion {
  val version = "1.1"
}
case object Xml10 extends XmlVersion {
  val version = "1.0"
}

trait DefaultXmlVersion {
  implicit val defaultVersion : XmlVersion = Xml10
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
    else 
      true

  /**
   * Validates and returns or throws
   */
  def validateLocalName( validLocal : String ) ( implicit ver : XmlVersion) = 
    if (validLocalName(validLocal)) 
      validLocal
    else
      error("The local name '"+validLocal+"' is not valid")

}

import QNameCharUtils._

/**
 * Seperate the notion of a normal Namespace and that of the "empty namespace" - no default namespace
 */ 
sealed trait UnderlyingNamespace {
  val uri : String
  override def hashCode = uri.hashCode
  override def equals( other : Any ) = other match {
    case ons : UnderlyingNamespace => 
      (this eq ons) || (uri == ons.uri) 
    case _ => false
  }
}

/**
 * Represents a XML Namespace spec compliant Namespace.
 *
 * NOTE Users are recommended to use the prefixed function and work with prefixes directly for qnames.
 */
trait Namespace extends UnderlyingNamespace {

  /**
   * Create an UnprefixedQName 
   */ 
  def apply( local : String )(implicit ver : XmlVersion, fromParser : FromParser) = UnprefixedQName(local, this)

  /**
   * Create a PrefixedQName directly
   */ 
  def apply( prefix : String, local : String)(implicit ver : XmlVersion, fromParser : FromParser) = PrefixedQName(local, prefixed(prefix))

  /**
   * Create a PrefixedNamespace
   */ 
  def prefixed( prefix : String )(implicit ver : XmlVersion, fromParser : FromParser) = PrefixedNamespace(this, prefix)

  /**
   * Create a PrefixedQName directly
   */ 
  def prefixed( prefix : String, local : String)(implicit ver : XmlVersion, fromParser : FromParser) : PrefixedQName = PrefixedQName(local, prefixed(prefix))

}

/**
 * Special case for empty namespaces
 */ 
object EmptyNamespace extends UnderlyingNamespace {
  final val uri = ""
}

object Namespace {
  val xmlnsNS = "http://www.w3.org/XML/1998/namespace"
  val xmlNS = "http://www.w3.org/2000/xmlns/"

  private[xml] def swapForKnown( validUri : String ) =
    validUri match {
      case `xmlnsNS` => xmlnsNS
      case `xmlNS` => xmlNS
      case _ => validUri
    }

  /**
   * parsers we trust, users we protect
   */ 
  def apply( validUri : String )(implicit ver : XmlVersion, fromParser : FromParser) : Namespace = new Namespace {
    val uri = 
      if (fromParser eq NotFromParser) 
	if (validXmlNamespace(validUri))
	  swapForKnown(validUri)
	else
	  error("Namespaces must have valid URIs, '"+validUri+"' is invalid")
      else
	swapForKnown(validUri)
  }
}

sealed trait PrefixedNamespace  {
  val ns : Namespace
  val prefix : String
  def apply( local : String )(implicit ver : XmlVersion, fromParser : FromParser) = PrefixedQName(local, this)

  override def hashCode = 
    (ns.hashCode * 31) + prefix.hashCode

  override def equals( other : Any ) = other match {
    case opns : PrefixedNamespace => ns == opns.ns && prefix == opns.prefix
    case _ => false
  }
}

object PrefixedNamespace {

  val xmlnsPRE = "xmlns"
  val xmlPRE = "xml"

  private[xml] def swapForKnown( nsprefix : String ) = 
    nsprefix match {
      case `xmlnsPRE` => xmlnsPRE
      case `xmlPRE` => xmlPRE
      case _ => nsprefix
    }

  private[xml] def verifyConstraint( pre : String, ns : Namespace, 
				    agpre : String, agns : String) { 
    if ((ns.uri eq agns) && (pre ne agpre)) {
	error("The namespace '"+agns+"' can only be bound to prefix '"+agpre+"'")
    }
  }

  private[xml] def checkPrefix( nsprefix : String, ns : Namespace )(implicit ver : XmlVersion) = 
    if (validXmlPrefix(nsprefix)) {
      val x = swapForKnown(nsprefix)
      verifyConstraint(x, ns, xmlPRE, Namespace.xmlNS)
      verifyConstraint(x, ns, xmlnsPRE, Namespace.xmlnsNS)
      x
    } else error("The prefix '"+nsprefix+"' is not valid")

  def apply(namespace : Namespace, nsprefix : String)(implicit ver : XmlVersion, fromParser : FromParser) : PrefixedNamespace = new PrefixedNamespace {
    val ns = namespace
    val prefix =
      if (fromParser eq NotFromParser) 
	checkPrefix(nsprefix, namespace)
      else 
	swapForKnown(nsprefix)
  }
}

object Default {
  /**
   * placeholder for the current element default namespace
   */
  protected[xml] val namespace = Namespace("")(Xml10,IsFromParser)

  /**
   * The no namespace namespace (xmlns="")
   */
  val noNamespace = EmptyNamespace
}
