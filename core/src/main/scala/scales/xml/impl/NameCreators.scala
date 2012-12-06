package scales.xml.impl

import scales.xml.{QName, AttributeQName}
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
