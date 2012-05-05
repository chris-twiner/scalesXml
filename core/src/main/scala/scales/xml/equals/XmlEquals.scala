package scales.xml.equals

import scales.xml.{PullType, QName, Elem, Attribs, Attributes, Attribute, XmlItem, XmlPath, EndElem, XCC, NamespaceContext, Misc, Miscs}

import scala.collection.immutable.{ Stack, Map }

import scalaz._
import Scalaz._

object BasicPaths {
  // {ns}Local -> count
  type BasicPathA = (QName, Map[String, Int])
  type BasicPath = List[BasicPathA] 
}

import BasicPaths._

/**
 * Base functions for equality
 */ 
trait XmlEquals {

  /**
   * Allows comparison of Text nodes or Attribute values that contain QNames, i.e. prefix:value.  In order to perform this comparison both the left and right sides must have a NamespaceContext.
   */ 
  def qnamesEqual(context : ComparisonContext, str : String, str2 : String) = {
    // split both, if one has and the other not, then its false anyway
    val sp1 = str.split(":")
    val sp2 = str2.split(":")
    if (sp1.size == 2 && sp2.size == 2) {
      sp1(1) == sp2(1) && { // values match
	// look up prefixes
	(for{ lnc <- context.leftNamespaceContext
	     rnc <- context.rightNamespaceContext
	     lns <- lnc.mappings.get(sp1(0))
	     rns <- rnc.mappings.get(sp2(0))
	   } yield lns == rns
        ).
	getOrElse(false)
      }
    } else 
      str == str2
  }

  /**
   * Perform an actual token comparison (Text/CData and Attribute value relevant).
   */ 
  def compareTokens( context : ComparisonContext, qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean], str : String, str2 : String) =
    qnameTokenComparison.
      map( f =>
	f(context, str, str2)
	).
      getOrElse(str == str2)


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
  with QNameEquals
  with DefaultQNameToken {
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
  with QNameEquals
  with DefaultQNameToken
  with DefaultDocLikeEquals {
}

object DefaultXmlEquals extends DefaultXmlEquals {}

/**
 * Makes the given path the top path
 */ 
class PathAsPullTypeIterable( originalPath : XmlPath ) extends scales.utils.AbstractPathIterator[XmlItem, Elem, XCC, PullType] {

  def initialPath : XmlPath = originalPath.copy( top = scales.utils.Top() )

  def event : PullType = path.node.focus.fold(x=>x,y=>y.section)

  def end = {
    val el = path.tree.section
    EndElem(el.name, el.namespaces) : PullType
  }
 
}

/**
 * For Iterator[PullType]s that actually are, lets help the inference and implicit lookup out
 */ 
trait TheyReallyAreIterators {
  import scales.xml.{CloseablePull, XmlPull}
  implicit val closeablePullIsAn = (x : CloseablePull) => x : Iterator[PullType]
  implicit val xmlPullIsAn = (x : XmlPull) => x : Iterator[PullType]
}

/**
 * Collection of all implicit conversions to StreamComparables.
 *
 * NOTE: The results are only usable with compare / ===, and should not be used to serialize
 */
trait StreamComparableImplicits extends TheyReallyAreIterators {
  import scales.xml.{CloseablePull, XmlPull, DocLike, Doc, XmlTree}

  implicit val itrPlusDocAsAnIterator = (x : (Iterator[PullType], DocLike)) => x._1 : Iterator[PullType]

  /**
   * Converts directly to a StreamComparable, its not generally a good idea to automagically  mix XmlPath as an Iterable with XmlPath as an Iterator, make it explicit if thats really desired. 
   */ 
  implicit val xmlPathToComparable : XmlPath => StreamComparable[XmlPath] = ( x : XmlPath ) => new StreamComparable[XmlPath](x)(t => new PathAsPullTypeIterable(t))

  /**
   * Converts XmlTree and DslBuilder (when used with PullTypeConversionImplicits 
   */ 
  implicit def fromStreamToStreamComparable[T <% Iterator[PullType]](t : T) : StreamComparable[T] = 
    new StreamComparable(t)

  /**
   * One off for (Iterator, DocLike)
   */ 
  implicit def itrDocLikeToStreamComparable[T <% Iterator[PullType]](t : (T, DocLike)) : StreamComparable[T] = new StreamComparable( t._1 )

  /**
   * Wrapper for Docs
   */
  implicit def docWrapper(implicit bodyComp : XmlComparison[XmlTree]) : DocLikeWrapper[Doc] = new DocLikeWrapperBase[Doc, XmlTree]( identity, _.rootElem, bodyComp )

  /**
   * Wrapper for XmlPull
   */ 
  implicit def xmlPullWrapper(implicit bodyComp : XmlComparison[Iterator[PullType]]) : DocLikeWrapper[XmlPull] = new DocLikeWrapperBase[XmlPull, Iterator[PullType]]( identity, identity, bodyComp )

  /**
   * Wrapper for CloseablePull
   */
  implicit def closeablePullWrapper(implicit bodyComp : XmlComparison[Iterator[PullType]]) : DocLikeWrapper[CloseablePull] = new DocLikeWrapperBase[CloseablePull, Iterator[PullType]]( identity, identity, bodyComp )

  /**
   * Wrapper for (Iterator, DocLike)
   */ 
  implicit def itrDocLikeWrapper(implicit bodyComp : XmlComparison[Iterator[PullType]]) : DocLikeWrapper[(Iterator[PullType], DocLike)] = new DocLikeWrapperBase[(Iterator[PullType], DocLike), Iterator[PullType]]( _._2, _._1, bodyComp )
}
