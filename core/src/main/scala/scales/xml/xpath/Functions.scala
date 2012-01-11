package scales.xml.xpath

import scales.xml._
import scales.utils._

/**
 * Type class representing Xml objects which provide qnames.  Most of the
 * functions allow for implicit scope to ease use in xpaths.
 */ 
trait Names[T] {
  /**
   * Returns the localName
   */ 
  def localName(implicit t : T) : String

  /** curried to allow direct drop in for predicates, if it is an item then it will return false */
  def localName(localName : String)(implicit t : T ) : Boolean
  
  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def exact(qname : QName)(implicit t : T ) : Boolean

  /**
   * Matches on prefix and namespace only
   */ 
  def equivalent(qname : QName)(implicit t : T ) : Boolean

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname(implicit t : T) : String

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName(implicit t : T) : String

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName(implicit t : T) : String
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
   * localName_== for XmlPaths
   */ 
  def localNameX_==( local : String ) : XmlPath => Boolean =
    (x : XmlPath) =>XmlPathNames.localName(local)(x)

  /**
   * localName_== for AttributePaths
   */ 
  def localNameA_==( local : String ) : AttributePath => Boolean =
    (x : AttributePath) =>AttributePathNames.localName(local)(x)
}

/**
 * Functions providing access to QNames
 */ 
trait NameFunctions {
  /**
   * Returns the localName
   */ 
  def localName[T](implicit t : T, name : Names[T], d : DIF) : String =
    name.localName

  /**
   * Returns the localName
   */ 
  def localName[T](t : T)(implicit name : Names[T]) : String =
    name.localName(t)

  /**
   * curried to allow direct drop in for predicates, if it is an item then it will return false
   */
  def localName_==[T](localName : String)(implicit name : Names[T] ) : T => Boolean =
    (t : T) => name.localName(localName)(t)
  
  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def exact[T](qname : QName)(implicit t : T, name : Names[T], d : DIF ) : Boolean =
    name.exact(qname)

  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def exact[T](qname : QName)(t : T)(implicit name : Names[T] ) : Boolean =
    name.exact(qname)(t)

  /**
   * Matches on prefix and namespace only
   */ 
  def equivalent[T](qname : QName)(implicit t : T, name : Names[T], d : DIF ) : Boolean = 
    name.equivalent(qname)
    
  /**
   * Matches on prefix and namespace only
   */ 
  def equivalent[T](qname : QName, t : T)(implicit name : Names[T] ) : Boolean = 
    name.equivalent(qname)(t)

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname[T](implicit t : T, name : Names[T], d : DIF) : String = 
    name.qname

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname[T](t : T)(implicit name : Names[T]) : String = 
    name.qname(t)

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](implicit t : T, name : Names[T], d : DIF) : String =
    name.qualifiedName

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](t : T)(implicit name : Names[T]) : String =
    name.qualifiedName(t)

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](implicit t : T, name : Names[T], d : DIF) : String =
    name.pqName

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](t : T)(implicit name : Names[T]) : String =
    name.pqName(t)
}

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
}

/**
 * Base impl for QNames
 */ 
trait QNameUsers[T] extends Names[T] {

  implicit def convert(t : T) : QName

  /**
   * Returns the localName
   */ 
  def localName(implicit t : T) : String = 
    t.local

  /** curried to allow direct drop in for predicates, if it is an item then it will return false */
  def localName(localName : String)(implicit t : T ) : Boolean = 
    t.local == localName
  
  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def exact(qname : QName)(implicit t : T ) : Boolean = 
    t === qname

  /**
   * Matches on prefix and namespace only
   */ 
  def equivalent(qname : QName)(implicit t : T ) : Boolean =
    t =:= qname

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qname(implicit t : T) : String =
    t.qName

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName(implicit t : T) : String =
    t.qualifiedName

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName(implicit t : T) : String =
    t.pqName
}

object AttributeNames extends QNameUsers[Attribute] {
  def convert(t : Attribute) : QName = EqualsHelpers.toQName(t.name)
}

object AttributePathNames extends QNameUsers[AttributePath] {
  def convert(t : AttributePath) : QName = EqualsHelpers.toQName(t.attribute.name)
}

object ElemNames extends QNameUsers[Elem] {
  def convert(t : Elem) : QName = t.name
}

object XmlTreeNames extends QNameUsers[XmlTree] {
  def convert(t : XmlTree) : QName = t.section.name
}

object XmlPathNames extends QNameUsers[XmlPath] {
  def convert(t : XmlPath) : QName = t.tree.section.name
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
