package scales.xml.dsl

import scales.utils.{AsBoolean, booleanAndTMatcher, booleanMatcher}
import scales.xml.{Attribute, Elem, Namespace, QName, ScalesXml, XmlTree}

/**
 * Matches a given QName against either an Attribute or Elem with the namespace and localname only (QName.=:=)
 */ 
class QNameMatcher(qname : QName) {
  import ScalesXml._
  /**
   * Returns Some(elem) when the QName matches namespace and localname, None otherwise
   */
  def unapply( elem : Elem ) : Option[Elem] = if (elem.name =:= qname) Some(elem) else None
  /**
   * Returns Some(attrib) when the QName matches namespace and localname, None otherwise
   */
  def unapply( attrib : Attribute ) : Option[Attribute] = if (toQName(attrib.name) =:= qname) Some(attrib) else None
}

/**
 * Leveraged via implicit fromQNameToQNamePimper, adds m/matcher to create a QNameMatcher
 */
class QNameMPimper( qname : QName ) {
  def m = new QNameMatcher(qname)
  def matcher = m
}

/**
 * Matches a given Namespace against either an Attribute or Elem with the namespace only
 */ 
class NamespaceMatcher(ns : Namespace) {
  import ScalesXml._
  /**
   *Returns Some(elem) when the QName's namespace matches, None otherwise
   */ 
  def unapply( elem : Elem ) : Option[Elem] = 
    if (elem.name.namespace == ns) Some(elem) else None
  /**
   * Returns Some(attrib) when the QName's namespace matches, None otherwise
   */
  def unapply( attrib : Attribute ) : Option[Attribute] = 
    if (toQName(attrib.name).namespace == ns) Some(attrib) else None
}


/**
 * Leveraged via implicit fromNSToNSMPimper, adds m/matcher to create a NamespaceMatcher
 */
class NSMPimper( ns : Namespace ) {
  def m = new NamespaceMatcher(ns)
  def matcher = m
}

/**
 * PathMatching functions over booleans
 */ 
trait XPathMatcher {

  /**
   * Creates a booleanMatcher against a Tree based on the @pathEval function.
   *
   * For XPath evaluations on the tree this typically gives the paths back when the xpath evaluates to a non-empty result.  TODO ADD CODE
   */ 
  def pathMatcher[T : AsBoolean]( pathEval : (XmlTree) => T ) = booleanMatcher[XmlTree, T](pathEval)

  
  /**
   * Similar to pathMatcher but also gives the tree back into the pattern match.
   */
  def pathAndTreeMatcher[T : AsBoolean]( pathEval : (XmlTree) => T ) = booleanAndTMatcher[XmlTree, T](pathEval)

}
