package scales.xml.equals

import scales.xml.{QName, Elem, Attribs, Attributes, Attribute, XmlItem, Text, PI, CData, Comment, PullType, EqualsHelpers, EndElem}

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

trait DefaultItemEquals {

  /**
   * Help inference out
   * 
   */ 

  implicit object defaultXmlItemComparison extends XmlComparison[XmlItem] {
    def compare( calculate : Boolean , context : ComparisonContext, left : XmlItem, right : XmlItem) : Option[(XmlDifference[_], ComparisonContext)] = {
      def check( str : String, str2 : String ) =
	  if (str == str2) None
	  else {
	    if (calculate) 
	      Some((ItemDifference(left, right), context))
	    else
	      noCalculation
	  }

      (left, right) match { // we have to do it on types as well
	case (Text(valu), Text(value)) => check(valu, value)
	case (Comment(com), Comment(comm)) => check(com, comm)
	case (CData(cd), CData(cda)) => check(cd, cda)
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
  
}

object ItemEquals extends DefaultItemEquals {}

/**
 * Comparison between attributes, requires an Equal type class for QNames
 */ 
class AttributeComparison(implicit eqn : Equal[QName]) extends XmlComparison[Attribute] {
  import EqualsHelpers.toQName

  def compare( calculate : Boolean, context : ComparisonContext, left: Attribute, right : Attribute) : Option[(XmlDifference[_], ComparisonContext)] = {
    if (!eqn.equal(toQName(left.name), toQName(right.name)))
      if (calculate)
	Some((AttributeNameDifference( left, right), context))
      else
	noCalculation
    else 
      if (left.value != right.value)
	if (calculate)
	  Some((AttributeValueDifference( left, right), context))
	else
	  noCalculation
      else
	None // a ok
  }
}


trait DefaultAttributeEquals {
  /**
   * QNames are not compared with prefix
   */ 
  implicit def defaultAttributeComparison(implicit qe : Equal[QName]) : XmlComparison[Attribute] = new AttributeComparison()(qe)
  
}

object AttributeEquals extends DefaultAttributeEquals with DefaultQNameEquals {}

trait ExactQName{
  /**
   * QNames are compared with prefix
   */ 
  implicit val prefixAttributeComparison = new AttributeComparison()(equal { (a: QName, b: QName) => a.====(b) })
  
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

  val defaultStreamComparison : XmlComparison[StreamComparable[_]] = new StreamComparison()( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison, EqualsHelpers.qnameEqual)

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

  val defaultStreamComparison : XmlComparison[StreamComparable[_]] = new StreamComparison(joinTextAndCData _)( ItemEquals.defaultXmlItemComparison, ElemEquals.defaultElemComparison, EqualsHelpers.qnameEqual)

}
