package scales.xml.equals

import scales.xml.{QName, Elem, Attribs, Attributes, Attribute, XmlItem, Text, PI, CData, Comment, PullType, EndElem, Misc, Miscs, DocLike, impl}

import impl.EqualsHelpers

import XmlEquals._

import scalaz._
import Scalaz._

import SomeDifference.noCalculation

/**
 * This interface allows for non xml matching, for example a forward Iterator from a Text child.  This would be impossible to model as an xml document, but could be useful for comparison.
 *
 * This trait boxes a given conversion, stopping accidental serialize calls on the resulting streams.
 */ 
class StreamComparable[T <% Iterator[PullType]]( val t : T ) {
  def underlyingIterator : Iterator[PullType] = t
}

/**
 * Compares based on streams.  Requires comparisons for XmlItem, Elem and a QName Equal instance for testing EndElems.
 */ 
class StreamComparison( filter : Iterator[PullType] => Iterator[PullType] = identity)( implicit ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], endElemQNameEqual : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) extends XmlComparison[StreamComparable[_]] {
  def compare( calculate : Boolean, ocontext : ComparisonContext , lefti : StreamComparable[_], righti : StreamComparable[_] ) : Option[(XmlDifference[_], ComparisonContext)] = {

    var res : Option[(XmlDifference[_], ComparisonContext)] = None
    val joined = filter(lefti.underlyingIterator).zip(filter(righti.underlyingIterator))
    var context = ocontext
    var streamPosition = 0

    while( res.isEmpty && joined.hasNext) {
      streamPosition += 1

      joined.next match {
	case (Left(x : Elem), Left(y : Elem)) =>
	  if (calculate || qnameTokenComparison.isDefined) { // also needed for qname handling
	    context = context.startElems(x, y)
	  }
	  res = ec.compare(calculate, context, x, y)
	case (Left(x : XmlItem), Left(y : XmlItem)) =>
	  res = ic.compare(calculate, context, x, y)
	case (Right(x : EndElem), Right(y  : EndElem)) =>
	  res = 
	    if (!endElemQNameEqual.equal(x.name, y.name)) {
	      if (calculate)
		Some((EndElemNameDifference(x, y), context))
	      else 
		noCalculation
	    } else None
	  if (calculate) { // also needed for qname handling
	    context = context.endElem
	  }
	case (left, right) => 
	  res = 
	    if (calculate || qnameTokenComparison.isDefined)
	      Some((DifferentTypes(left, right), context)) 
	    else
	      noCalculation
      }
    }
    if (calculate && res.isDefined) {
      // unpack, repack
      val Some((diff, rcontext)) = res
      context = rcontext.withPosition(streamPosition)
      res = Some((diff, context))
    }

    res
  }
}

/**
 * Wraps a given T with a conversion from T to an xml stream
 */ 
class StreamComparisonWrapper[T <% StreamComparable[T]]( val str : StreamComparison ) extends XmlComparison[T] {
  def compare( calculate : Boolean, context : ComparisonContext, lt : T, rt : T) : Option[(XmlDifference[_], ComparisonContext)] = 
    str.compare(calculate, context, lt, rt)
}

trait StreamEquals {
  
  /**
   * Conversions
   */
  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[T]
}

trait ExactStreamEquals extends StreamEquals {
  /**
   * Conversions
   */
  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[T] = new StreamComparisonWrapper(new StreamComparison()( ic, ec, qe, qnameTokenComparison))

}

object ExactStreamEquals extends ExactStreamEquals {
  import QNameEquals._
  import AttributeEquals._
  import AttributesEquals._

  def defaultStreamComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[StreamComparable[_]] = new StreamComparison()( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison(AttributesEquals.defaultAttributesComparison(AttributeEquals.defaultAttributeComparison(EqualsHelpers.qnameEqual, qnameTokenComparison)), EqualsHelpers.qnameEqual), EqualsHelpers.qnameEqual, qnameTokenComparison)

}

/**
 * Streams compared after transforming via joinTextAndCData
 */ 
trait DefaultStreamEquals extends StreamEquals {
  import LogicalFilters.joinTextAndCData

  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[T] = new StreamComparisonWrapper( new StreamComparison(joinTextAndCData _)( ic, ec, qe, qnameTokenComparison) )

}

object DefaultStreamEquals extends DefaultStreamEquals {
  import LogicalFilters.joinTextAndCData
  import QNameEquals._
  import AttributeEquals._
  import AttributesEquals._

  def defaultStreamComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[StreamComparable[_]] = new StreamComparison(joinTextAndCData _)( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison(AttributesEquals.defaultAttributesComparison(AttributeEquals.defaultAttributeComparison(EqualsHelpers.qnameEqual, qnameTokenComparison)), EqualsHelpers.qnameEqual), EqualsHelpers.qnameEqual, qnameTokenComparison)
}

/**
 * Wrap the creation of doclike things
 */ 
abstract class DocLikeWrapper[T]( val tToDoc : T => DocLike ){
  
  /**
   * Compare the bodies, this hiding of the comparison is a bit more verbose at the definition site, but helps implicit lookup immensely
   */ 
  def compare( calculate : Boolean, context : ComparisonContext, leftBody : T, rightBody : T ) : Option[(XmlDifference[_], ComparisonContext)]
}

/**
 * Base wrapper for most usecases, extra type is here to keep lookup working
 */ 
class DocLikeWrapperBase[T, B]( tToDoc : T => DocLike, tToBody : T => B, bodyComp : XmlComparison[B]) extends DocLikeWrapper[T](tToDoc) {
  def compare( calculate : Boolean, context : ComparisonContext, leftBody : T, rightBody : T ) : Option[(XmlDifference[_], ComparisonContext)] =
    bodyComp.compare(calculate, context, tToBody(leftBody), tToBody(rightBody))
}

/**
 * Compares neither of the version, DTD nor encoding of a document, but prolog and end misc.
 * Requires a @T that can be converted to a DocLike and a body @B.  B must have an XmlComparison available and there must exist conversions from T to DocLike and T to B.
 */ 
class DocLikeComparison[T, B](implicit ic : XmlComparison[XmlItem], docWrapper : DocLikeWrapper[T]) extends XmlComparison[T] {

  implicit val tToDoc = docWrapper.tToDoc

  def compare( calculate : Boolean, context : ComparisonContext , left : T, right : T ) : Option[(XmlDifference[_], ComparisonContext)] = {
// should we split out XmlComparison[Misc]?, its only used by DocLike right now..
    def compareMiscs( lmiscs : Miscs, rmiscs : Miscs , prolog : Boolean) = 
      if (lmiscs.size != rmiscs.size)
	if (calculate) 
	  Some((DifferentNumberOfMiscs(lmiscs, rmiscs, prolog), context))
	else
	  noCalculation
      else 
	scales.utils.collectFirst(lmiscs.zip(rmiscs)){
	  case (a, b) => 
	    val l : XmlItem = a.fold( x => x, y => y)
	    val r : XmlItem = b.fold( x => x, y => y)
	    val res = ic.compare(calculate, context, l, r)
	    if (calculate && res.isDefined)
	      // give back either the diff or wrap the difference
	      res.map{ 
		case (ItemDifference(x, y), cont) => 
		  (MiscDifference(a, b, prolog), cont)
		case (DifferentTypes(x, y), cont) =>
		  (MiscDifferentTypes(a, b, prolog), cont)
		case t => t
	      } 
	    else
	      res
	}
      
    compareMiscs(left.prolog.misc, right.prolog.misc, true).
      orElse(docWrapper.compare(calculate, context, left, right)).
      orElse(compareMiscs(left.end.misc, right.end.misc, false))
  }
}

trait DefaultDocLikeEquals {
  /**
   * Provides the comparison for prolog, body and end miscs
   */ 
  implicit def defaultDocLikeComparison[T](implicit ic : XmlComparison[XmlItem], docWrapper : DocLikeWrapper[T]) : XmlComparison[T] = new DocLikeComparison()(ic, docWrapper)
}

