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
  def name(implicit t : T) : Option[QName]

  /**
   * simplify the usage in Names
   */ 
  protected[xml] def flatName(implicit t : T) : QName =
    name(t).getOrElse(EmptyQName.empty)
}

// Note - 2.8.x doesn't allow multiple implicit lists so we must combine them and duplicate the interface

/**
 * Dummy implicit filler for easing interface issues
 */ 
class DIF()

/**
 * Represents an empty qname for those cases that should return empty string
 */
protected[xml] object EmptyQName {
  /**
   * Both namespace and localname are empty
   */ 
  protected[xml] val empty = NoNamespaceQName("")(Xml10, IsFromParser)
}

/**
 * Collects all type class based xpath functions, exposed via Functions in package
 *
 * Also adds aliases for the common functions
 */ 
trait Functions extends NameFunctions with TextFunctions {
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

  private[NameFunctions] implicit def toQNameNF[T]( t : T )(implicit name : Names[T]) =
    name.flatName(t)
  
  /**
   * Returns the localName
   */ 
  def localName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.local

  /**
   * Returns the localName
   */ 
  def localName[T](t : T)(implicit name : Names[T]) : String =
    localName(t, name, DIF.dif)

  /**
   * curried to allow direct drop in for predicates, if it is an item then it will return false
   */
  def hasLocalName[T](localName : String)(implicit name : Names[T] ) : T => Boolean =
    (t : T) => t.local == localName
  
  /**
   * Does the qname match exactly (prefix included if present)
   */ 
  def isExactly[T](qname : QName)(implicit name : Names[T] ) :  T => Boolean =
    (t : T) => t ==== qname
    
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
    qname(t, name, DIF.dif)

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qName[T](implicit t : T, name : Names[T], d : DIF) : String = 
    qname(t, name, DIF.dif)

  /**
   * Returns the XPath QName - prefix:local or local
   */ 
  def qName[T](t : T)(implicit name : Names[T]) : String = 
    qname(t, name, DIF.dif)

  /**
   * We cannot allow the QName to escape if its invalid.
   */ 
  private[this] def testNonEmpty( r : QName ) = 
    if (r eq EmptyQName.empty) 
      error("A Names instance has returned the EmptyQName.  This is not directly accessable")
    else 
      r

  /**
   * Returns the QName, will throw if the QName is "empty"
   */ 
  def name[T](implicit t : T, name : Names[T], d : DIF) : QName = 
    testNonEmpty(t)

  /**
   * Returns the QName, will throw if the QName is "empty"
   */ 
  def name[T](t : T)(implicit iname : Names[T]) : QName = 
    name(t, iname, DIF.dif)

  /**
   * Will be true for all values of T except when the resulting QName is "empty".
   *
   * If hasQName is false then calling name will throw
   */ 
  def hasQName[T](implicit t : T, name : Names[T], d : DIF) : Boolean = 
    name.name(t).isDefined

  /**
   * Will be true for all values of T except when the resulting QName is "empty".
   *
   * If hasQName is false then calling name will throw
   */ 
  def hasQName[T](t : T)(implicit name : Names[T]) : Boolean = 
    hasQName(t, name, DIF.dif)

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.qualifiedName

  /**
   * Returns the qualified name {namespace}local
   */ 
  def qualifiedName[T](t : T)(implicit name : Names[T]) : String =
    qualifiedName(t, name, DIF.dif)

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](implicit t : T, name : Names[T], d : DIF) : String =
    t.pqName

  /**
   * Returns either qualifiedName or prefix:{namespace}local when a prefix is present
   */ 
  def pqName[T](t : T)(implicit name : Names[T]) : String =
    pqName(t, name, DIF.dif)

  /**
   * Returns the underlying namespace object
   */ 
  def namespace[T](implicit t : T, name : Names[T], d : DIF) : UnderlyingNamespace =
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
  def hasNamespace[T](namespaceUri : String)(implicit name : Names[T], d: DIF) : T => Boolean =
    (t : T) => t.namespace.uri == namespaceUri

}

trait FunctionImplicits extends TextImplicits with NamesImplicits

object DIF {
  // only used to seperate the interfaces, fully implicit gets this as well
  val dif = new DIF()
}

/**
 * Name type classes
 */ 
trait NamesImplicits {
  // only used to seperate the interfaces, fully implicit gets this as well
  implicit val dif = DIF.dif

  implicit val attribNames = AttributeNames
  implicit val attributePathNames = AttributePathNames
  implicit val xpathNames = XmlPathNames
  implicit val elemNames = ElemNames
  implicit val xtreeNames = XmlTreeNames 
  implicit val qnameNames = QNameNames
  implicit val aqnameNames = AQNameNames
  implicit val dslNames = DslNames

  implicit def attribPathsNames[T <: Iterable[XmlPath]] = AttributePathsNames.asInstanceOf[Names[AttributePaths[T]]]
  implicit def xpathToNames[T <: Iterable[XmlPath]] = XPathNames.asInstanceOf[Names[XPath[T]]]
}

object AttributeNames extends Names[Attribute] {
  def name(implicit t : Attribute) : Option[QName] = Some(EqualsHelpers.toQName(t.name))
}

object AttributePathsNames extends Names[AttributePaths[_]] {
  def name(implicit a : AttributePaths[_]) = {
    val r = a.attributes
    r.headOption.map( x => EqualsHelpers.toQName( x.attribute.name ))
  }
}

object AttributePathNames extends Names[AttributePath] {
  def name(implicit t : AttributePath) : Option[QName] = Some(EqualsHelpers.toQName(t.attribute.name))
}

object ElemNames extends Names[Elem] {
  def name(implicit t : Elem) : Option[QName] = Some(t.name)
}

object XmlTreeNames extends Names[XmlTree] {
  def name(implicit t : XmlTree) : Option[QName] = Some(t.section.name)
}

object DslNames extends Names[DslBuilder] {
  def name(implicit t : DslBuilder) : Option[QName] = Some(t.toTree.section.name)
}

object XmlPathNames extends Names[XmlPath] {
  def name(implicit t : XmlPath) : Option[QName] = Some(t.tree.section.name)
}

object QNameNames extends Names[QName] {
  def name(implicit t : QName) : Option[QName] = Some(t) 
}

object AQNameNames extends Names[AttributeQName] {
  def name(implicit t : AttributeQName) : Option[QName] = Some(EqualsHelpers.toQName(t))
}

object XPathNames extends Names[XPath[_]] {
  def name(implicit a : XPath[_]) = {
    val r = ScalesXml.fromXPathToIterable(a) 
    r.headOption.flatMap(_.focus(_ => None, t => XmlTreeNames.name(t)))
  }
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
    text(t, value, DIF.dif)

  /**
   * More readable version for XmlItems and Attributes, same as text
   */ 
  def value[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    text(t, value, DIF.dif)
 
  /**
   * XPath normalize-space function, replaces all consecutive whitespace with " " and trims.
   */
  def normalizeSpace[T](implicit t : T, value : TextValue[T], d : DIF ) : String =
    normalizeSpaceS(value.text)

  /**
   * The text value of a given object, .value for attributes & items, the accumalated text if its an elem
   */
  def text[T](t : T)(implicit value : TextValue[T]) : String =
    text(t, value, DIF.dif)

  /**
   * XPath name for text
   */ 
  def string[T](t : T)(implicit value : TextValue[T]) : String =
    text(t, value, DIF.dif)

  /**
   * More readable version for XmlItems and Attributes, same as text
   */ 
  def value[T](t : T)(implicit value : TextValue[T] ) : String =
    text(t, value, DIF.dif)
 
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
  implicit val xmlpathText = XmlPathText
  implicit val itemText = XmlItemText
  implicit val itemOrElemText = ItemOrElemText
  implicit val dslText = DslText

  implicit def attribPathsText[T <: Iterable[XmlPath]] = AttributePathsText.asInstanceOf[TextValue[AttributePaths[T]]]
  implicit def xpathToTextValue[T <: Iterable[XmlPath]] = XPathText.asInstanceOf[TextValue[XPath[T]]]
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

object DslText extends TextValue[DslBuilder] {
  def text(implicit t : DslBuilder) = XmlTreeText.text(t.toTree)
}

object XmlPathText extends TextValue[XmlPath] {
  def text(implicit t : XmlPath) = 
    t.focus(_.value, XmlTreeText.text(_))
}

object ItemOrElemText extends TextValue[ItemOrElem] {
  def text(implicit t : ItemOrElem) =
    t.fold(_.value, XmlTreeText.text(_))
}

object AttributeText extends TextValue[Attribute] {
  def text(implicit a : Attribute) = a.value
}

object AttributePathText extends TextValue[AttributePath] {
  def text(implicit a : AttributePath) = a.attribute.value
}

object AttributePathsText extends TextValue[AttributePaths[_]] {
  def text(implicit a : AttributePaths[_]) =
    if (a.attributes.size == 0) ""
    else a.attributes.head.attribute.value  
}

object XmlItemText extends TextValue[XmlItem] {
  def text(implicit a : XmlItem) = a.value
}

object XPathText extends TextValue[XPath[_]] {
  def text(implicit a : XPath[_]) = {
    val r = ScalesXml.fromXPathToIterable(a) 
    if (r.size == 0) ""
    else XmlPathText.text( r.head )
  }
}
