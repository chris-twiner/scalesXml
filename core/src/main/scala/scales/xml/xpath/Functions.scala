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
 */ 
object Functions extends NameFunctions with TextFunctions {}

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

  /** curried to allow direct drop in for predicates, if it is an item then it will return false */
  def localName[T](localName : String)(t : T)(implicit name : Names[T] ) : Boolean =
    name.localName(localName)(t)

  /** curried to allow direct drop in for predicates, if it is an item then it will return false */
  def localName[T](localName : String)(implicit t : T, name : Names[T], d : DIF ) : Boolean =
    name.localName(localName)    
  
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
  def equivalent[T](qname : QName)(t : T)(implicit name : Names[T] ) : Boolean = 
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

trait NamesImplicits {
  // only used to seperate the interfaces, fully implicit gets this as well
  implicit val dif = new DIF()

  implicit val elemNames = ElemNames
  implicit val xtreeNames = XmlTreeNames 
  implicit val attribNames = AttributeNames
  implicit val xpathNames = XmlPathNames
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
  implicit val xpathText = XmlPathText
  implicit val itemText = XmlItemText
}

trait TextTrees[T] extends TextValue[T] {
  def convert(t : T) : XmlTree

  def text(implicit t: T): String = {
    convert(t).fold(new StringBuilder()) 
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

object XmlTreeText extends TextTrees[XmlTree] {
  def convert(t : XmlTree) : XmlTree = t
}

object XmlPathText extends TextTrees[XmlPath] {
  def convert(t : XmlPath) : XmlTree = t.tree
}

object AttributeText extends TextValue[Attribute] {
  def text(implicit a : Attribute) = a.value
}

object XmlItemText extends TextValue[XmlItem] {
  def text(implicit a : XmlItem) = a.value
}
