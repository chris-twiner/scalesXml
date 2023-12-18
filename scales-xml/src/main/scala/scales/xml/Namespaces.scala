package scales.xml

import scales.utils.error
import scales.utils.{LeftLike, RightLike}

import scales.xml.impl.{FromParser, NotFromParser, IsFromParser}

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

import impl.QNameCharUtils._

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
sealed trait Namespace extends UnderlyingNamespace {

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

private[xml] object NamespaceImpl {
  import Namespace._

  def swapForKnown( validUri : String )(implicit ver : XmlVersion, fromParser : FromParser) =
    validUri match {
      case `xmlnsNS` => xmlns
      case `xmlNS` => xml
      case `xsiNS` => xsi
      case _ => new Namespace{ val uri = validUri }
    }
}

object Namespace {
  val xmlNS = "http://www.w3.org/XML/1998/namespace"
  val xmlnsNS = "http://www.w3.org/2000/xmlns/"
  val xsiNS = "http://www.w3.org/2001/XMLSchema-instance"

  val xmlns: Namespace = new Namespace{ val uri = xmlnsNS }
  val xml: Namespace  = new Namespace{ val uri = xmlNS }
  val xsi: Namespace  = new Namespace{ val uri = xsiNS }

  import NamespaceImpl.swapForKnown

  /**
   * parsers we trust, users we protect
   */ 
  def apply( validUri : String )(implicit ver : XmlVersion, fromParser : FromParser) : Namespace = 
    if (fromParser eq NotFromParser) 
      if (validXmlNamespace(validUri))
	swapForKnown(validUri)
      else
	error("Namespaces must have valid URIs, '"+validUri+"' is invalid for Xml "+ver.version)
    else
      swapForKnown(validUri)

  def unapply( n : Namespace) = Some((n.uri))
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
    if (((ns.uri eq agns) && (pre ne agpre)) ||
      ((pre eq agpre) && (ns.uri ne agns))) {
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

  def unapply( p : PrefixedNamespace) = Some((p.ns, p.prefix))
}
