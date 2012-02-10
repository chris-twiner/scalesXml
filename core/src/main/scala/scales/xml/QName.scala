package scales.xml

import scales.utils.error
import scales.utils.{LeftLike, RightLike}

import QNameCharUtils._

/**
 * QNames together with a tree structure form the basis of XML, what type of QName is available depends on Attribute (either no namespace or prefxied - fully qualified) or Elem (attribute's options and non prefixed but still qualified)
 */ 
sealed trait QName {

  val local : String

  /**
   * There is always a namespace, but it may be the noNamespace namespace
   */ 
  val namespace : UnderlyingNamespace
  val prefix : Option[String]
  
  /**
   * When the version is 1.1 it cannot be serialized to a 1.0 doc.
   * This indicates with which version it is compatible
   */ // to get here it must be valid so we only need to test for 1.0
  def qNameVersion : XmlVersion = 
    if (validLocalName(local)(Xml10) && 
	// treat non prefixed as Xml10 prefixes as they are compatible with both
	(prefix.map{p => validXmlPrefix(p)(Xml10)}.getOrElse(true)) &&
	((namespace eq EmptyNamespace)  ||
	  validXmlNamespace(namespace.uri)(Xml10) ))
      Xml10
    else 
      Xml11

  def hasPrefix = prefix.isDefined

  /**
   * local or prefix:local Namespaces QName, also XPath 2.0 functions version of a qualified name
   */
  def qName = 
    if (prefix.isDefined) 
      prefix.get + ":" + local
    else local
  
  /**
   * QName, pre:{namespace}localName JClark representation with prefix: on the front if it has a prefix.  CTw's own, for when you just really want that prefix.
   */
  def pqName = if (prefix.isDefined) prefix.get +":" + qualifiedName else qualifiedName

  /**
   * QName, {namespace}localName JClark representation, as per everywhere else than w3c
   */
  def qualifiedName = "{" + namespace.uri + "}" + local

  override def toString = qualifiedName

  /** 
    * Will match namespace and local with other AND prefix if available
    */
  def ===( other : QName) = 
    if (this eq other) true
    else
    if (this =:= other) {
      val hp = hasPrefix
      val ohp = other.hasPrefix

      if (hp && ohp)
    	prefix.get == other.prefix.get
      else 
	(hp == ohp) // both false
    } else false   

  /** 
   * Will match namespace and local with other but not prefix === also does prefix
   */
  def =:=( other : QName) = 
    (other eq this) ||
    (local == other.local && namespace.uri == other.namespace.uri)
  
  /**
   * Matches using =:= and does not work with === or prefixes
   */ 
  override def equals( other : Any ) = other match {
    case oq : QName => this =:= oq
    case _ => false
  }

  override def hashCode() : Int = {
    var hs = 1
    hs = (hs * 31) + prefix.map(_.hashCode).getOrElse(1) // None is good here too
    hs = (hs * 31) + local.hashCode
    hs = (hs * 31) + namespace.hashCode
    hs    
  }
}

/**
 * Mixed in to Prefixed and Unprefixed
 */ 
trait CanHavePrefix extends QName {

  val namespace : Namespace

  def withPrefix( prefix : String )(implicit ver : XmlVersion, fromParser: FromParser) : PrefixedQName =
      PrefixedQName( local , namespace.prefixed(prefix))
}

/**
 * Has neither a prefix nor a namespace (e.g. <fred xmlns=""/>)
 */
trait NoNamespaceQName extends QName with RightLike[PrefixedQName, NoNamespaceQName] {
  final val prefix = None
  final val namespace = Default.noNamespace
}

object NoNamespaceQName {
  def apply(locali : String)(implicit ver: XmlVersion, fromParser : FromParser) = new NoNamespaceQName {
    if (fromParser eq NotFromParser) {
      validateLocalName(locali)
    }

    final val local = locali
  }

  def unapply(n : NoNamespaceQName) = Some((n.local))
}

/**
 * Has both a namespace and a prefix (e.g. <pre:fred xmlns:pre="uri"/>)
 */ 
trait PrefixedQName extends QName with CanHavePrefix with LeftLike[PrefixedQName, NoNamespaceQName]

object PrefixedQName {
  def apply( locali : String, prefixedNamespace : PrefixedNamespace)(implicit ver: XmlVersion, fromParser : FromParser) = new PrefixedQName {
    if (fromParser eq NotFromParser) {
      validateLocalName(locali)
    }
    final val local = locali
    final val prefix = Some(prefixedNamespace.prefix)
    final val namespace = prefixedNamespace.ns
  }
  def unapply(n : PrefixedQName) = Some((n.local, n.prefix.get, n.namespace))
/*  def unapply(n : PrefixedQName) = 
    Some((n.local, 
	  PrefixedNamespace(n.namespace, n.prefix.get)
	  (n.qNameVersion, IsFromParser)))
  */
}

/**
 * Has no prefix but a namespace (e.g. <fred xmlns="uri"/>)
 */ 
trait UnprefixedQName extends QName with CanHavePrefix {
  val prefix = None
}

object UnprefixedQName {

  def apply(locali : String, namespacei : Namespace)(implicit ver: XmlVersion, fromParser : FromParser) = new UnprefixedQName {
    if (fromParser eq NotFromParser) {
      validateLocalName(locali)
    }
    final val local = locali
    final val namespace = namespacei
  }

  def unapply(n : UnprefixedQName) = Some((n.local, n.namespace))
}

trait QNameImplicits {

  implicit def stringToNoNamespace( localOnly : String )(implicit ver : XmlVersion, fromParser: FromParser) = NoNamespaceQName(localOnly)

  implicit def localStringToNSBuilder( local : String)(implicit ver : XmlVersion, fromParser: FromParser) = new StringToNSBuilder(local)
}

/**
 * Pimps a string for namespace handling
 */ 
class StringToNSBuilder(local: String)(implicit ver : XmlVersion, fromParser: FromParser) {

  /**
   * ns :: localname pimp.
   */ 
  def ::( namespace : String ) = if ((namespace eq null) || namespace == "") error("Namespace should not be non empty or null") else UnprefixedQName(local, Namespace(namespace))

  /**
   * When there are other implicits after the string conversion gold add an "l" for local only 
   */
  def l(implicit ver : XmlVersion, fromParser : FromParser) = NoNamespaceQName(local)

  /**
   * When there are other implicits after the string conversion gold add an "localOnly" or "l" for local only 
   */
  def localOnly(implicit ver : XmlVersion, fromParser : FromParser) = NoNamespaceQName(local)
}
