package scales.xml.equals

import scales.xml.{PullType, QName, Elem, Attribs, Attributes, Attribute, XmlItem, XmlPath, EndElem, XCC}

import scalaz._
import Scalaz._

/**
 * Why did equality fail, pattern match fun
 */ 
sealed trait XmlDifference[X] {
  val left : X
  val right : X
}

trait BasicPaths {
  // {ns}Local -> count
  type BasicPathA = (QName, Map[String, Int])
  type BasicPath = List[BasicPathA] 
}

object BasicPaths extends BasicPaths {}

/**
 * Provides an immplicit to easily access the head QName in a path
 */ 
trait BasicPathsImplicits {
  import BasicPaths._
  
  implicit object BasicPathNames extends scales.xml.xpath.Names[BasicPath] {
    def name(implicit t : BasicPath) : Option[scales.xml.QName] = 
      t.headOption.map( _._1 )
  }

}

/**
 * Like Equals but also gives a path in addition to the fun reason
 * 
 */ 
trait XmlComparison[T] {
  import BasicPaths._
  /**
   * Takes the path for information reasons (works for streams as well).  The return is either None == boolean or the reason
   */ 
  def compare( calculate : Boolean , path : BasicPath, left : T, right : T) : Option[(XmlDifference[_], BasicPath)]
}

/**
 * Base functions for equality
 */ 
trait XmlEquals extends BasicPaths {

  /**
   * An implicit but its only purpose is to convert, and needs the given comparison to function, which is provided (or not) by ScalesXml
   */ 
  implicit def fromCompToEq[T](implicit comp : XmlComparison[T]) : Equal[T] = 
    equal {
      ( a : T, b : T) =>
	comp.compare(false, Nil, a, b).isEmpty
    }

  /**
   * Pushes a new elem on the stack, modifying the parents counts as it goes
   */ 
  def startElem( qname : QName, path : BasicPath ) : BasicPath =
    if (path.isEmpty) 
      scales.utils.one((qname, Map[String, Int]()))
    else {
      val (h :: r) = path
      val qn = qname.qualifiedName
      val count = h._2.get(qn).getOrElse(0)
      (qname, Map[String, Int]()) :: 
      (h._1, h._2.updated(qn, count + 1)) :: r
    }

  /**
   * Produces an XPath like string.  Make this a full one?
   */ 
  def pathString( path : BasicPath ) : String = {
    var str = ""
    var l = path//.reverse
    while( ! l.isEmpty ) {
      val head :: tail = l
      l = tail

      val qn = head._1.qualifiedName
      val count =
	if (tail.isEmpty) 1 
	else {
          val nhead :: rest = tail
	  nhead._2(qn)
	}
      
      str = "/"+ qn + "["+ count +"]" + str
    }
    str
  }

  /**
   * path.tail, pops the head off as it moves up, counts are not changed
   */ 
  def endElem( path : BasicPath ) : BasicPath = 
    path.tail

  /**
   * Compare the xml object via the available XmlDifference type class
   */ 
  def compare[T : XmlComparison]( path : BasicPath, left : T, right : T) : Option[(XmlDifference[_], BasicPath)] =
    implicitly[XmlComparison[T]].compare(true, path, left, right)
}

/**
 * Make it available without dragging the rest of the world in
 */ 
object XmlEquals extends XmlEquals {
}

import XmlEquals._

/**
 * Magik object for when we aren't attempting to calculate whats wrong 
 */ 
protected[xml] object SomeDifference extends XmlDifference[AnyRef] {
  val left = null
  val right = null

  def unapply(a : XmlDifference[AnyRef]) : Option[(AnyRef, AnyRef)] = Some((left,right))

  val noCalculation : Option[(XmlDifference[_], BasicPath)] = Some((this, Nil))
}

/**
 * When types are different, or an end element vs XmlEvent
 */ 
case class DifferentTypes( left : PullType, right : PullType ) extends XmlDifference[PullType]

case class QNameDifference( left : QName, right : QName ) extends  XmlDifference[QName]

sealed trait AttributeDifference extends XmlDifference[Attribute]
sealed trait ElemDifference extends XmlDifference[Elem]
sealed trait AttributesDifference extends XmlDifference[Attributes]

case class AttributeNameDifference( left : Attribute, right : Attribute ) extends AttributeDifference
case class AttributeValueDifference( left : Attribute, right : Attribute ) extends AttributeDifference

/**
 * Their size is different
 */ 
case class DifferentNumberOfAttributes( left : Attributes, right : Attributes ) extends AttributesDifference

/**
 * Re-packs the AttributeValueDifference, but will only occur when the attributes QName is present
 */
case class DifferentValueAttributes( left : Attributes, right : Attributes, differentValue : Attribute ) extends AttributesDifference

/**
 * The size is the same but one of lefts qnames is not present in right
 */ 
case class MissingAttributes( left : Attributes, right : Attributes, missing : Attribute ) extends AttributesDifference

case class ItemDifference( left : XmlItem, right : XmlItem) extends XmlDifference[XmlItem]

case class ElemNameDifference( left : Elem, right : Elem ) extends ElemDifference

/**
 * If the difference is due to an attribute thats included as well
 */ 
case class ElemAttributeDifference( left : Elem, right : Elem, attributesDifference : AttributesDifference ) extends ElemDifference

/**
 * This can only be returned when an ElemComparisom has been suplied with a custom namespaces implementation.
 */ 
case class ElemNamespacesDifference( left : Elem, right : Elem ) extends ElemDifference

case class EndElemNameDifference( left : EndElem, right : EndElem ) extends XmlDifference[EndElem]

/**
 * All default exact Xml Equal and XmlComparison trait instances.
 *
 * CData, Comments, PI are all kept as is, DTD, encoding and prolog etc are not.  Text nodes are not joined.  Use LogicalXmlEquals to focus more on content only. 
 */ 
trait ExactXmlEquals 
  extends DefaultItemEquals
  with DefaultAttributeEquals
  with DefaultAttributesEquals 
  with DefaultElemEquals
  with ExactStreamEquals
  with QNameEquals {
}

/**
 * Provides simple access to ExactXmlEquals
 */ 
object ExactXmlEquals extends ExactXmlEquals {}

/**
 * All CData nodes are converted to text nodes, adjoining Text nodes (including CData) are joined.
 */
trait DefaultXmlEquals 
  extends DefaultItemEquals
  with DefaultAttributeEquals
  with DefaultAttributesEquals 
  with DefaultElemEquals
  with DefaultStreamEquals
  with QNameEquals {
}

object DefaultXmlEquals extends DefaultXmlEquals {}

class PathAsPullTypeIterable( val initialPath : XmlPath ) extends scales.utils.AbstractPathIterator[XmlItem, Elem, XCC, PullType] {

  def event : PullType = path.node.focus.fold(x=>x,y=>y.section)

  def end = {
    val el = path.tree.section
    EndElem(el.name, el.namespaces) : PullType
  }
 
}

/**
 * Collection of all implicit conversions to StreamComparables.
 *
 * NOTE: The results are only usable with compare / ===, and should not be used to serialize
 */
trait StreamComparableImplicits {

  /**
   * Converts directly to a StreamComparable, its not generally a good idea to automagically  mix XmlPath as an Iterable with XmlPath as an Iterator, make it explicit if thats really desired. 
   */ 
  implicit val xmlPathToComparable : XmlPath => StreamComparable[XmlPath] = ( x : XmlPath ) => new StreamComparable[XmlPath](x)(t => new PathAsPullTypeIterable(t))

  /**
   * Converts XmlTree and DslBuilder (when used with PullTypeConversionImplicits 
   */ 
  implicit def fromStreamToStreamComparable[T <% Iterator[PullType]](t : T) : StreamComparable[T] = 
    new StreamComparable(t)
    
}
