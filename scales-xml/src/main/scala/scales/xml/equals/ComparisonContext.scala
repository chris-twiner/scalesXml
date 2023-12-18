package scales.xml.equals

import scales.xml.{PullType, QName, Elem, Attribs, Attributes, Attribute, XmlItem, XmlPath, EndElem, XCC, Misc, Miscs}
import scales.xml.serializers.NamespaceContext

import scala.collection.immutable.{ Stack, Map }

import BasicPaths._

/**
 * ComparisonContext represents both the path to a given comparison and the namespace declarations which are relevant for it.
 *
 * @param streamPosition is incremented for each processed item in the stream, allowing a simple take to get the difference in terms of the stream, the developer can then use this with toDifferenceAsStream to capture the difference. It is only provided when a difference is found within a stream and not during the difference analysis.
 */
class ComparisonContext private (private val lnc : NamespaceContext = null, 
				 private val rnc : NamespaceContext = null, 
	 val path : BasicPath, private val parent : ComparisonContext = null, val streamPosition : Int = 0) {

  def withPosition( newStreamPosition : Int ) = 
    new ComparisonContext(lnc, rnc, path, parent, newStreamPosition)

  /**
   * Provides a stream for the given T up to the difference represented by this context.  Callers are responsible for handling any IO handles or capturing an original stream.
   */
  def toDifferenceAsStream[T](t : T, filter : Iterator[PullType] => Iterator[PullType] = LogicalFilters.joinTextAndCData(_))(implicit tv : T => StreamComparable[T]) : Stream[PullType] = filter( tv(t).underlyingIterator ).take(streamPosition).toStream

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

  val empty = apply()

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
