package scales.xml

import scales.utils._
import scala.collection.generic.CanBuildFrom
import scales.utils.one

trait XmlPathImplicits {
  implicit val samePath : (XmlPath, XmlPath) => Boolean = comparePathsDirect _

  /**
   * Don't force a re-evaluation each time
   */
  implicit val xpathEqual =
    ScalesUtils.toEqual[XmlItem, Elem, XCC]

  /** Unpack the attribute from the tuple */
  implicit def fromAttrPathToAttribute(attrPath : AttributePath) : Attribute = attrPath.attribute

  /** By default a List, eager/strict evaluation, is used.  The user of the library can simply choose another collection to wrap the xmlPath */
  implicit def fromXmlPathToXPath(xmlPath : XmlPath)(
    implicit cbf : CanBuildFrom[List[XmlPath], XmlPath, List[XmlPath]]) : XPath[List[XmlPath]] =
    new XPath[List[XmlPath]](
      XPathInfo(one(one(xmlPath)), initialNode = true, eager = true), cbf)
  //      XPathInfo(one(one(xmlPath).view).view, initialNode = true), cbf)

  implicit def fromXPathToIterable(xpath : XPath[_]) : Iterable[XmlPath] = {
    val nodes = xpath.path.nodes.flatten
    if (nodes.size < 2) nodes // sorting on one or 0 still costs
    else
      DuplicateFilter(sort[XmlItem, Elem, XCC](paths = nodes)(ScalesXml.xpathSortingClassManifest))
  }

  /**
   * Sort in document order.
   */
  implicit def fromAttributePathsToIterable[T <: Iterable[XmlPath]](attrs : AttributePaths[T]) : Iterable[AttributePath] =
    if (attrs.attributes.size < 2) attrs.attributes
    else
      sortT[XmlItem, Elem, XCC, AttributePath](attrs.attributes.map { x => (x, x.parent) }).map { x => x._1 }

  implicit def fromPathToComparisoms(path : XmlPath) = XmlPathComparisoms(path)

  implicit def fromAPathToAComparisoms(path : AttributePath) = AttributePathComparisoms(path)

  /**
   * Mimic the logic of xpath boolean() through typeclass, see AsBoolean / XmlDSL
   */

  implicit val XPathToBoolean = (x : XPath[_]) => !x.path.nodes.isEmpty

  implicit val AttributePathsToBoolean = (x : AttributePaths[_]) => !x.attributes.isEmpty

  implicit val IterableToBoolean = (x : Iterable[_]) => !x.isEmpty

  implicit val StringToBoolean = (x : String) => x.length > 0

  implicit val NumberToBoolean = (x : Number) => x.longValue() > 0

  implicit val BooleanToBoolean = (x : Boolean) => x

}

trait XmlPaths {

  /**
   * Normally the implicit fromXPathToIterable will provide sorted output, but this can create significant slow down when only the contents are necessary, not their order.
   *
   * Use raw to convert without forcing sorting or checking for duplicates.
   *
   * NOTE Any laziness aquired by viewed will also be lost by the flatten
   */
  def raw[T <: Iterable[XmlPath]](xpath : XPath[T]) : Iterable[XmlPath] =
    xpath.path.nodes.flatten

  /**
   * Normally the implicit fromXPathToIterable will provide sorted output, but this can create significant slow down when only the contents are necessary, not their order.
   *
   * Use lazyRaw to convert without forcing sorting or checking for duplicates, and to evaluate lazily.
   *
   * Warning, don't use this version of raw unless your XPath usage costs more than the navigation, its based on Stream and terribly slow (huge memory and stack usage etc).
   *
   * NOTEs
   * Laziness with flatten is achieved by using iterator.toIterable before the flatten (uses Stream internally).
   * Its only useful as a lazy evaluator if viewed was used.
   */
  def lazyRaw[T <: Iterable[XmlPath]](xpath : XPath[T]) : Iterable[XmlPath] =
    xpath.path.nodes.iterator.toIterable.flatten

  /**
   * Lazy evaluation of XPaths
   *
   * View called on nested Lists, but not on xlmPath.
   */
  def viewed(xmlPath : XmlPath)(
    implicit cbf : CanBuildFrom[List[XmlPath], XmlPath, List[XmlPath]]) : XPath[List[XmlPath]] =
    new XPath[List[XmlPath]](
      XPathInfo(one(one(xmlPath).view).view, initialNode = true), cbf)

  import ScalesXml._

  /**
   * Same as fromXmlPathToXPath, an eager evaluation of xpath queries
   */
  def eager(xmlPath : XmlPath)(
    implicit cbf : CanBuildFrom[List[XmlPath], XmlPath, List[XmlPath]]) : XPath[List[XmlPath]] =
    fromXmlPathToXPath(xmlPath)

  val isText = (x : XmlPath) => x.isItem == true && (x.item().isInstanceOf[Text] || x.item().isInstanceOf[CData])

  /** returns the attributes of a given element */
  def attributes(implicit path : XmlPath) : Attributes = path.tree.section.attributes

  /** returns the Elem at this path */
  def elem(implicit path : XmlPath) : Elem = path.tree.section

  val isItem = (x : XmlPath) => x.isItem
  val isElem = (x : XmlPath) => !x.isItem
}
