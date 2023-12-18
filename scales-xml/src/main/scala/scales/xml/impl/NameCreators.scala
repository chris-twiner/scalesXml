package scales.xml.impl

import scales.xml.{QName, AttributeQName, UnprefixedQName, Namespace, NoNamespaceQName, XmlVersion}

import scales.xml.parser.strategies.{MemoryOptimisationStrategy, OptimisationToken}

/**
 * Given an optimisation strategy create QNames for elements and attributes.
 *
 * These are primarily of use from parsers and handle 0 length uris and qNames with ":" in them.
 */ 
object NameCreators {

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
  def ::( namespace : String ) = if ((namespace eq null) || namespace == "") scales.utils.error("Namespace should not be non empty or null") else UnprefixedQName(local, Namespace(namespace))

  /**
   * When there are other implicits after the string conversion gold add an "l" for local only 
   */
  def l(implicit ver : XmlVersion, fromParser : FromParser) = NoNamespaceQName(local)

  /**
   * When there are other implicits after the string conversion gold add an "localOnly" or "l" for local only 
   */
  def localOnly(implicit ver : XmlVersion, fromParser : FromParser) = NoNamespaceQName(local)
}
