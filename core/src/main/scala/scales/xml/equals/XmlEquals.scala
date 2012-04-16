package scales.xml.equals

import scales.xml.{PullType, QName, Elem, Attribs, Attributes, Attribute, XmlItem, XmlPath, EndElem, XCC, NamespaceContext}

import scala.collection.immutable.{ Stack, Map }

import scalaz._
import Scalaz._

/**
 * Why did equality fail, pattern match fun
 */ 
sealed trait XmlDifference[X] {
  val left : X
  val right : X
}

object BasicPaths {
  // {ns}Local -> count
  type BasicPathA = (QName, Map[String, Int])
  type BasicPath = List[BasicPathA] 
}

import BasicPaths._

/**
 * ComparisonContext represents both the path to a given comparison and the namespace declarations which are relevant for it.
 */ 
class ComparisonContext private (private val lnc : NamespaceContext = null, 
				 private val rnc : NamespaceContext = null, 
	 val path : BasicPath, private val parent : ComparisonContext = null) {

  import ComparisonContext.emptyWithDefault

  def leftNamespaceContext : Option[NamespaceContext] = 
    Option(lnc)

  def rightNamespaceContext : Option[NamespaceContext] = 
    Option(rnc)

  /**
   * Pushes a new elem on the stack, modifying the parents counts as it goes
   */ 
  def startElems( leftElem : Elem, rightElem : Elem ) : ComparisonContext = 
    new ComparisonContext(
      scales.xml.doElement(leftElem, 
			   if (lnc eq null) 
			     emptyWithDefault
			   else
			     lnc.mappings)
    , scales.xml.doElement(rightElem, 
			   if (rnc eq null) 
			     emptyWithDefault
			   else
			     rnc.mappings)
    , if (path.isEmpty)
	scales.utils.one((leftElem.name, Map[String, Int]()))
      else {
	val qname = leftElem.name

	val (h :: r) = path
	val qn = qname.qualifiedName
	val count = h._2.get(qn).getOrElse(0)
	(qname, Map[String, Int]()) :: 
	(h._1, h._2.updated(qn, count + 1)) :: r
      }
    , this)

  /**
   * Produces an XPath like string.  Make this a full one?
   */ 
  def pathString : String = {
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
  def endElem : ComparisonContext = 
    if (parent eq null) 
      error("Cannot call endElem on a root context")
    else
      // the path's count must be kept, everything else should come from the 
      // parent
      new ComparisonContext(parent.lnc, parent.rnc, path.tail, parent.parent)
  
  def parentContext : Option[ComparisonContext] = 
    Option(parent)
}

object ComparisonContext {
  protected[xml] val emptyWithDefault = Map(""->"")

  /**
   * Creates a new context with the given starting path
   */ 
  def apply(path : BasicPath) : ComparisonContext =
    new ComparisonContext(null, null, path, null)

  def apply() : ComparisonContext =
    new ComparisonContext(null, null, Nil, null)

  def unapply( c : ComparisonContext ) : Option[(Option[NamespaceContext], Option[NamespaceContext], BasicPath, Option[ComparisonContext])] = 
    Some(c.leftNamespaceContext, c.rightNamespaceContext, c.path,
	 c.parentContext)
}

/**
 * Provides an immplicit to easily access the head QName in a path
 */ 
trait ComparisonContextImplicits {
  
  implicit object BasicPathNames extends scales.xml.xpath.Names[BasicPath] {
    def name(implicit t : BasicPath) : Option[scales.xml.QName] = 
      t.headOption.map( _._1 )
  }

  implicit object ComparisonContextNames extends scales.xml.xpath.Names[ComparisonContext] {
    def name(implicit t : ComparisonContext) : Option[scales.xml.QName] = 
      t.path.headOption.map( _._1 )
  }

}

/**
 * Like Equals but also gives a path in addition to the fun reason
 * 
 */ 
trait XmlComparison[T] {

  /**
   * Takes the context for information reasons (works for streams as well).  The return is either None == boolean or the reason
   */ 
  def compare( calculate : Boolean , context : ComparisonContext, left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)]
}

/**
 * Base functions for equality
 */ 
trait XmlEquals {

  /**
   * An implicit but its only purpose is to convert, and needs the given comparison to function, which is provided (or not) by ScalesXml
   */ 
  implicit def fromCompToEq[T](implicit comp : XmlComparison[T]) : Equal[T] = 
    equal {
      ( a : T, b : T) =>
	comp.compare(false, ComparisonContext(), a, b).isEmpty
    }

  /**
   * Compare the xml object via the available XmlDifference type class
   */ 
  def compare[T : XmlComparison]( left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)] =
    implicitly[XmlComparison[T]].compare(true, ComparisonContext(), left, right)

  /**
   * Compare the xml object via the available XmlDifference type class
   */ 
  def compare[T : XmlComparison]( context : ComparisonContext, left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)] =
    implicitly[XmlComparison[T]].compare(true, context, left, right)
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

  val noCalculation : Option[(XmlDifference[_], ComparisonContext)] = Some((this, ComparisonContext()))
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
