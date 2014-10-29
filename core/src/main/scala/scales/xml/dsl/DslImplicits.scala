package scales.xml.dsl

import scales.utils.collection.path._
import scales.utils.{AsBoolean, subtree, foldPositions, booleanMatcher, booleanAndTMatcher, top, item}

import scales.xml.{
  NoNamespaceQName, PrefixedQName, AttributeQName, 
  Attribute, XmlTree, Doc, XmlVersion, XmlCBF, 
  XmlPath, Elem, XCC, XmlItem, QName, ItemOrElem,
  ScalesXml, Text, Namespace, <, ?<}

import ScalesXml.xmlCBF

import ScalesXml.fromParserDefault // note cannot be in parser here


trait DslImplicits {
  import ScalesXml._
  implicit def fromElemToBuilder(elem : Elem) = <(elem)
  implicit def fromQNamePairToAttribute(pair : (PrefixedQName, String)) = Attribute(pair._1, pair._2)

  implicit def fromDslBuilderToTree(dslB : DslBuilder) = dslB.toTree
  
  /**
   * Only works for elems, allows simpler definitions
   */
  implicit def fromQNameToTree( qname : QName) = DslBuilder.q2tree(qname)

  /**
   * Serialisation and other dsl friends benefit from this
   */ 
  implicit def fromElemToTree( elem : Elem ) : XmlTree = DslBuilder.elem2tree(elem)

  /**
   * Allows direct use of text where expected
   */ 
  implicit def fromStringToText( value : String ): Text = Text(value)

  /**
   * Only works for elems, better looking than <
   */
  implicit def fromQNameToBuilder( qname : QName) = <(qname)

  /**
   * matches elements and attributes based on qname only
   */
  implicit def fromQNameToQNamePimper( qname : QName) = new QNameMPimper(qname)

  implicit def fromTreeToDsl( tree: XmlTree ) = DslBuilder(tree)

  implicit def fromNSToNSMPimper( ns : Namespace ) = new NSMPimper(ns)
}

/**
 * Add ?-> to an attributeqname
 */ 
final class OptionalAttribute(val name: AttributeQName) {
  def ?->(value: String): Option[Attribute] =
    Some(Attribute(name, value))

  def ?->(value: Option[String]): Option[Attribute] =
    value.map(Attribute(name, _))
}

/**
 * DslImplicits centered on the OptionalDslBuilder only
 */ 
trait OptionalDslBuilderImplicits {

  implicit def fromElemToOptionalBuilder(elem : Elem) = ?<(elem)

  /**
   * Only works for elems, better looking than <
   */
  implicit def fromQNameToOptionalBuilder( qname : QName) = ?<(qname)

  implicit def fromTreeToODsl( tree: XmlTree ) = OptionalDslBuilder(tree)

  /**
   * Provides access to the ~> pimps
   */ 
  implicit def fromPQNameToOptionalAttribute(name: AttributeQName) = 
    new OptionalAttribute(name)

  /**
   * Convenience function for adding optional subtrees
   */ 
  implicit def fromOptionalDslToOptionalTree(optionalDsl: OptionalDslBuilder): Option[ItemOrElem] =
    optionalDsl.toOptionalTree

}
