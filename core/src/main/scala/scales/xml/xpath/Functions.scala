package scales.xml.xpath

import scales.xml._
import scales.utils._

/**
 * Type class representing Xml objects which provide qnames.  Most of the
 * functions allow for implicit scope to ease use in xpaths.
 */ 
trait Names[T] {

  /**
   * Returns the QName
   */ 
  def name(implicit t : T) : QName

}

// Note - 2.8.x doesn't allow multiple implicit lists so we must combine them and duplicate the interface

/**
 * Dummy implicit filler for easing interface issues
 */ 
class DIF()

/**
 * Collects all type class based xpath functions, exposed via Functions in package
 *
 * Also adds aliases for the common functions
 */ 
object Functions extends NameFunctions with TextFunctions {
  /**
   * hasLocalName for XmlPaths
   */ 
  def hasLocalNameX( local : String ) : XmlPath => Boolean =
    (x : XmlPath) => hasLocalName[XmlPath](local)(XmlPathNames)(x)

  /**
   * hasLocalName for AttributePaths
   */ 
  def hasLocalNameA( local : String ) : AttributePath => Boolean =
    (x : AttributePath) => hasLocalName[AttributePath](local)(AttributePathNames)(x)
}

/**
 * Functions providing access to QNames
 */ 
trait NameFunctions {

  private implicit def toQName[T]( t : T )(implicit name : Names[T]) =
    name.name(t)
  
  /**
   * Returns the localName
   */ 
  def localName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.local

  /**
   * Returns the localName
   */ 
  def localName[T](t : T)(implicit name : Names[T]) : String =
    t.local

  /**
   * curried to allow direct drop in for predicates, if it is an item then it will return false
   */
  def hasLocalName[T](localName : String)(implicit name : Names[T] ) : T => Boolean =
    (t : T) => t.local == localName
  
  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def isExactly[T](qname : QName)(implicit name : Names[T] ) :  T => Boolean =
    (t : T) => t === qname
    
  /**
   * Matches on prefix and namespace only
   */ 
  def isEquivalent[T](qname : QName)(implicit name : Names[T] ) : T => Boolean =
    (t : T) => t =:= qname

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname[T](implicit t : T, name : Names[T], d : DIF) : String = 
    t.qName

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname[T](t : T)(implicit name : Names[T]) : String = 
    t.qName

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qName[T](implicit t : T, name : Names[T], d : DIF) : String = 
    t.qName

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qName[T](t : T)(implicit name : Names[T]) : String = 
    t.qName

  /**
   * Returns the QName
   */ 
  def name[T](implicit t : T, name : Names[T], d : DIF) : QName = t

  /**
   * Returns the QName
   */ 
  def name[T](t : T)(implicit name : Names[T]) : QName = t

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.qualifiedName

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](t : T)(implicit name : Names[T]) : String =
    t.qualifiedName

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.pqName

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](t : T)(implicit name : Names[T]) : String =
    t.pqName

  /**
   * Returns the namespace object
   */ 
  def namespace[T](implicit t : T, name : Names[T], d : DIF) : Namespace =
    t.namespace

  /**
   * matches only the namespace
   */
  def hasNamespace[T](namespace : Namespace)(implicit name : Names[T]) : T => Boolean =
    (t : T) => t.namespace == namespace

  /**
   * XPath namespace-uri function, returns the uri
   */ 
  def namespaceUri[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.namespace.uri

  /**
   * XPath namespace-uri function, returns the uri
   */ 
  def namespaceUri[T](t : T)(implicit name : Names[T]) : String =
    t.namespace.uri

  /**
   * matches only the namespace
   */
  def hasNamespace[T](namespaceUri : String)(implicit name : Names[T]) : T => Boolean =
    (t : T) => t.namespace.uri == namespaceUri

}

trait FunctionImplicits extends TextImplicits with NamesImplicits

/**
 * Name type classes
 */ 
trait NamesImplicits {
  // only used to seperate the interfaces, fully implicit gets this as well
  implicit val dif = new DIF()

  implicit val attribNames = AttributeNames
  implicit val attributePathNames = AttributePathNames
  implicit val xpathNames = XmlPathNames
  implicit val elemNames = ElemNames
  implicit val xtreeNames = XmlTreeNames 
  implicit val qnameNames = QNameNames
  implicit val aqnameNames = AQNameNames
}

object AttributeNames extends Names[Attribute] {
  def name(implicit t : Attribute) : QName = EqualsHelpers.toQName(t.name)
}

object AttributePathNames extends Names[AttributePath] {
  def name(implicit t : AttributePath) : QName = EqualsHelpers.toQName(t.attribute.name)
}

object ElemNames extends Names[Elem] {
  def name(implicit t : Elem) : QName = t.name
}

object XmlTreeNames extends Names[XmlTree] {
  def name(implicit t : XmlTree) : QName = t.section.name
}

object XmlPathNames extends Names[XmlPath] {
  def name(implicit t : XmlPath) : QName = t.tree.section.name
}

object QNameNames extends Names[QName] {
  def name(implicit t : QName) : QName = t 
}

object AQNameNames extends Names[AttributeQName] {
  def name(implicit t : AttributeQName) : QName = EqualsHelpers.toQName(t) 
}

/**
 * Type class for text values
 */ 
trait TextValue[T] {
  /**
   * The text value of a given object, .value for attributes & items, the accumalated text if its an elem
   */
  def text(implicit t : T) : String
} 

trait TextFunctions {
  /**
   * The text value of a given object, .value for attributes & items, the accumalated text if its an elem
   */
  def text[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    value.text

  /**
   * XPath name for text
   */ 
  def string[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    value.text

  /**
   * More readable version for XmlItems and Attributes, same as text
   */ 
  def value[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    value.text
 
  /**
   * XPath normalize-space function, replaces all consecutive whitespace with " " and trims.
   */
  def normalizeSpace[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    normalizeSpaceS(value.text)

  /**
   * The text value of a given object, .value for attributes & items, the accumalated text if its an elem
   */
  def text[T](t : T)(implicit value : TextValue[T]) : String =
    value.text(t)

  /**
   * XPath name for text
   */ 
  def string[T](t : T)(implicit value : TextValue[T]) : String =
    value.text(t)

  /**
   * More readable version for XmlItems and Attributes, same as text
   */ 
  def value[T](t : T)(implicit value : TextValue[T] ) : String =
    value.text(t)
 
  /**
   * XPath normalize-space function, replaces all consecutive whitespace with " " and trims.
   */
  def normalizeSpace[T](t : T)(implicit value : TextValue[T] ) : String =
    normalizeSpaceS(value.text(t))
}

trait TextImplicits {
  implicit val xtreeText = XmlTreeText
  implicit val attribText = AttributeText
  implicit val attribPathText = AttributePathText
  implicit val xpathText = XmlPathText
  implicit val itemText = XmlItemText
}

object XmlTreeText extends TextValue[XmlTree] {
  def text(implicit t: XmlTree): String = {
    t.fold(new StringBuilder()) 
    { (walker, sb) =>
      if (walker.isLeft) {
        walker.left.get match {
          case Text(text) => sb.append(text)
          case _ => ()
        }
      }
     sb
   }.toString
  }
}

object XmlPathText extends TextValue[XmlPath] {
  def text(implicit t : XmlPath) = 
    t.focus(_.value, XmlTreeText.text(_))
}

object AttributeText extends TextValue[Attribute] {
  def text(implicit a : Attribute) = a.value
}

object AttributePathText extends TextValue[AttributePath] {
  def text(implicit a : AttributePath) = a.attribute.value
}

object XmlItemText extends TextValue[XmlItem] {
  def text(implicit a : XmlItem) = a.value
}
