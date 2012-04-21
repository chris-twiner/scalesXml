package scales.xml.equals

import scales.xml.{QName, Elem, Attribs, Attributes, Attribute, XmlItem, Text, PI, CData, Comment, PullType, EqualsHelpers, EndElem, Misc, Miscs, DocLike}

import XmlEquals._

import scalaz._
import Scalaz._

import SomeDifference.noCalculation

trait DefaultQNameEquals {
  implicit val qnameEqual = equal { (a: QName, b: QName) => a =:= b }
}

/**
 * Only added to provide a complete `compare` set
 */ 
class QNameComparison( implicit qe : Equal[QName] ) extends XmlComparison[QName] {
  def compare( calculate : Boolean , context : ComparisonContext, left : QName, right : QName ) = {
    if (left === right)
      None
    else
      if (calculate)
	Some((QNameDifference(left, right), context))
      else
	noCalculation
  }
}

trait QNameEquals {
  implicit def qnameComparison( implicit qe : Equal[QName] ) : XmlComparison[QName] = new QNameComparison()(qe)
}

object QNameEquals extends DefaultQNameEquals with QNameEquals {
}

trait DefaultQNameToken {

  /**
   * Finally decides if a Text/CData node or Attribute value should be compared as if containing a qname (prefix:local). 
   */ 
  implicit val defaultQNameTokenComparison : Option[(ComparisonContext, String, String) => Boolean] = None

}

/**
 * Compares XmlItems, providing Some qnameTokenComparison will force that to be used to decide if qname comparison should be used or not
 */ 
class XmlItemComparison()(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) extends XmlComparison[XmlItem] {
  def compare( calculate : Boolean , context : ComparisonContext, left : XmlItem, right : XmlItem) : Option[(XmlDifference[_], ComparisonContext)] = {
    def check( str : String, str2 : String, isQNameRelevant : Boolean = false ) = {
      val res = 
	if (isQNameRelevant)
	  compareTokens(context, qnameTokenComparison, str, str2)
	else
	  str == str2 // qnames don't make sense for non Text / CData

      if (res) None
      else {
	if (calculate) 
	  Some((ItemDifference(left, right), context))
	else
	  noCalculation
      }
    }

    (left, right) match { // we have to do it on types as well
      case (Text(valu), Text(value)) => check(valu, value, true)
      case (Comment(com), Comment(comm)) => check(com, comm)
      case (CData(cd), CData(cda)) => check(cd, cda, true)
      case (PI(ta, valu), PI(tar, value)) => 
	check(ta, tar) orElse check(valu,  value)
      case _ => 
	if (calculate)
	  Some((DifferentTypes(left, right), context))
	else
	  noCalculation
    }
  }
}

trait DefaultItemEquals {

  /**
   * creates an XmlItem comparison given a given qnameTokenComparison function (should it compare using qnames or not).
   */ 
  implicit def defaultXmlItemComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[XmlItem] = new XmlItemComparison()(qnameTokenComparison)
  
}

object ItemEquals extends DefaultItemEquals {}

/**
 * Comparison between attributes, requires an Equal type class for QNames
 */ 
class AttributeComparison(implicit eqn : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) extends XmlComparison[Attribute] {
  import EqualsHelpers.toQName

  def compare( calculate : Boolean, context : ComparisonContext, left: Attribute, right : Attribute) : Option[(XmlDifference[_], ComparisonContext)] = {
    if (!eqn.equal(toQName(left.name), toQName(right.name)))
      if (calculate)
	Some((AttributeNameDifference( left, right), context))
      else
	noCalculation
    else {
      if (!compareTokens( context, qnameTokenComparison, left.value, right.value ))
	if (calculate)
	  Some((AttributeValueDifference( left, right), context))
	else
	  noCalculation
      else
	None // a ok
    }
  }
}

trait DefaultAttributeEquals {
  /**
   * QNames are not compared with prefix
   */ 
  implicit def defaultAttributeComparison(implicit qe : Equal[QName], qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[Attribute] = new AttributeComparison()(qe, qnameTokenComparison)
  
}

object AttributeEquals extends DefaultAttributeEquals with DefaultQNameEquals {}

trait ExactQName{
  /**
   * QNames are compared with prefix
   */ 
  implicit def prefixAttributeComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) = new AttributeComparison()(equal { (a: QName, b: QName) => a.====(b) }, qnameTokenComparison)
  
}

/**
 * Comparisoms built on exact qname comparisom, prefixes can be important for some systems
 */ 
object ExactQName extends ExactQName {}

/**
 * Comparison for attributes, requires a comparison for individual attributes, allowing flexible definitions of equality
 */ 
class AttributesComparison( implicit ac : XmlComparison[Attribute]) extends XmlComparison[Attributes] {
  def compare( calculate : Boolean, context : ComparisonContext, left : Attributes, right : Attributes) : Option[(XmlDifference[_], ComparisonContext)] = {
    import EqualsHelpers._
    import scales.utils.collectFirst

    if (left.size != right.size)
      if (calculate)
	Some((DifferentNumberOfAttributes(left,right), context))
      else
	noCalculation
      else 
	// get the first error
	collectFirst[Attribute, (XmlDifference[_], ComparisonContext)](left){ 
	  x1 =>
	    right( x1.name ).cata( x2 => {// if we have it check the attributes
	      val r = ac.compare(calculate, context, x1, x2)
	      if (r.isDefined) {
		// it can only be AttributeValueDifference
		if (calculate)
		  Some( (DifferentValueAttributes( left, right, x1 ), context) )
		else
		  noCalculation
	      } else None
	    } , 
	    if (calculate)
	      Some( ( MissingAttributes( left, right, x1 ), context) )
	    else
	      noCalculation     
	    )
	}
  }
}

trait DefaultAttributesEquals {

  implicit def defaultAttributesComparison(implicit ac : XmlComparison[Attribute]) : XmlComparison[Attributes]  = new AttributesComparison()( ac )

}

object AttributesEquals extends DefaultAttributesEquals 
  with DefaultAttributeEquals with DefaultQNameEquals {}

object ElemEqualHelpers {
  type NamespacesEqual = Equal[Map[String, String]]
  val allwaysTrueNamespacesEqual = equal {
    ( a : Map[String, String], b : Map[String, String] ) => true
  }
}

/**
 * Namespaces by default are not compared.  They aren't marked as implicit as you really have to be explicitly wanting to compare them.
 *
 * An Attributes comparison and a QName Equal instance are required.  This also allows, for example, specification of equality using prefixes for the Elem, but not on the Attributes.
 */ 
class ElemComparison(namespaces : Equal[Map[String, String]] = ElemEqualHelpers.allwaysTrueNamespacesEqual)( implicit ac : XmlComparison[Attributes], eqn : Equal[QName]) extends XmlComparison[Elem] {
  def compare( calculate : Boolean, context : ComparisonContext, left : Elem, right : Elem) : Option[(XmlDifference[_], ComparisonContext)] = {

    if (!(left.name === right.name))
      if (calculate)
	Some((ElemNameDifference(left,right), context))
      else
	noCalculation
      else 
	ac.compare(calculate, context, left.attributes, right.attributes).
    cata( x => 
      if (calculate) {
	val (attrs : AttributesDifference, p) = x
	Some( (ElemAttributeDifference( left, right, attrs), context) )
      } else noCalculation
    ,
      if (!namespaces.equal(left.namespaces, right.namespaces))
	if (calculate)
	  Some( (ElemNamespacesDifference(left, right), context) )
	else
	  noCalculation
	else None
    )
  }
}


trait DefaultElemEquals {

  implicit def defaultElemComparison(implicit ae : XmlComparison[Attributes], qe : Equal[QName]) : XmlComparison[Elem] = new ElemComparison()( ae, qe )

}

object ElemEquals extends DefaultElemEquals
  with DefaultAttributesEquals
  with DefaultAttributeEquals with DefaultQNameEquals {}

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
class StreamComparison( filter : Iterator[PullType] => Iterator[PullType] = identity)( implicit ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], endElemQNameEqual : Equal[QName]) extends XmlComparison[StreamComparable[_]] {
  def compare( calculate : Boolean, ocontext : ComparisonContext , lefti : StreamComparable[_], righti : StreamComparable[_] ) : Option[(XmlDifference[_], ComparisonContext)] = {

    var res : Option[(XmlDifference[_], ComparisonContext)] = None
    val joined = filter(lefti.underlyingIterator).zip(filter(righti.underlyingIterator))
    var context = ocontext

    while( res.isEmpty && joined.hasNext) {
      joined.next match {
	case (Left(x : Elem), Left(y : Elem)) =>
	  context = context.startElems(x, y)
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
	  context = context.endElem
	case (left, right) => 
	  res = 
	    if (calculate)
	      Some((DifferentTypes(left, right), context)) 
	    else
	      noCalculation
      }
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
  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName]) : XmlComparison[T]
}

trait ExactStreamEquals extends StreamEquals {
  /**
   * Conversions
   */
  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName]) : XmlComparison[T] = new StreamComparisonWrapper(new StreamComparison()( ic, ec, qe))

}

object ExactStreamEquals extends ExactStreamEquals {
  import QNameEquals._
  import AttributeEquals._
  import AttributesEquals._

  def defaultStreamComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[StreamComparable[_]] = new StreamComparison()( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison, EqualsHelpers.qnameEqual)

}

/**
 * Streams compared after transforming via joinTextAndCData
 */ 
trait DefaultStreamEquals extends StreamEquals {
  import LogicalFilters.joinTextAndCData

  implicit def toDefaultStreamComparison[T](implicit tv : T => StreamComparable[T], ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], qe : Equal[QName]) : XmlComparison[T] = new StreamComparisonWrapper( new StreamComparison(joinTextAndCData _)( ic, ec, qe) )

}

object DefaultStreamEquals extends DefaultStreamEquals {
  import LogicalFilters.joinTextAndCData
  import QNameEquals._
  import AttributeEquals._
  import AttributesEquals._

  def defaultStreamComparison(implicit qnameTokenComparison : Option[(ComparisonContext, String, String) => Boolean]) : XmlComparison[StreamComparable[_]] = new StreamComparison(joinTextAndCData _)( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison, EqualsHelpers.qnameEqual)
}

/**
 * Compares neither of the version, DTD nor encoding of a document, but prolog and end misc.
 */ 
class DocLikeComparison(implicit ic : XmlComparison[XmlItem]) extends XmlComparison[DocLike] {

  def compare( calculate : Boolean, context : ComparisonContext , left : DocLike, right : DocLike ) : Option[(XmlDifference[_], ComparisonContext)] = {
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
		case t => t
	      } 
	    else 
	      res	 
	}
      
    compareMiscs(left.prolog.misc, right.prolog.misc, true).
      orElse(compareMiscs(left.end.misc, right.end.misc, false))    
  }
}

trait DefaultDocLikeEquals {
  /**
   * Provides the comparison for prolog and end miscs, but not the stream/doc itself.
   */ 
  implicit def defaultDocLikeComparison( implicit ic : XmlComparison[XmlItem] ) : XmlComparison[DocLike] = new DocLikeComparison()(ic)
}

