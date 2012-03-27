package scales.xml.equals

import scales.xml._

import scalaz._
import Scalaz._

import XmlEquals._

import SomeDifference.noCalculation

trait DefaultItemEquals {

  /**
   * Help inference out
   */ 
  implicit def toDefaultXmlItem[T <: XmlItem] : XmlComparison[T] = DefaultXmlItemComparison.asInstanceOf[XmlComparison[T]]

  implicit object DefaultXmlItemComparison extends XmlComparison[XmlItem] {
    def compare( calculate : Boolean , path : BasicPath, left : XmlItem, right : XmlItem) : Option[(XmlDifference[_], BasicPath)] = {
      def check( str : String, str2 : String ) =
	  if (str == str2) None
	  else {
	    if (calculate) 
	      Some((ItemDifference(left, right), path))
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
	    Some((DifferentTypes(left, right), path))
	  else
	    noCalculation
      }
    }
  }
  
  implicit val defaultXmlItem : Equal[XmlItem] = equal {
    (a : XmlItem, b : XmlItem) =>
      DefaultXmlItemComparison.compare(false, Nil, a, b).isEmpty 
  }
}

object ItemEquals extends DefaultItemEquals {}

/**
 * Comparison between attributes, requires an Equal type class for QNames
 */ 
class AttributeComparison(implicit eqn : Equal[QName]) extends XmlComparison[Attribute] {
  import ScalesXml.toQName

  def compare( calculate : Boolean, path : BasicPath, left: Attribute, right : Attribute) : Option[(XmlDifference[_], BasicPath)] = {
    if (!eqn.equal(toQName(left.name), toQName(right.name)))
      if (calculate)
	Some((AttributeNameDifference( left, right), path))
      else
	noCalculation
      else 
	if (left.value != right.value)
	  if (calculate)
	    Some((AttributeValueDifference( left, right), path))
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
  implicit val defaultAttributeComparison = new AttributeComparison()(ScalesXml.qnameEqual)
  
  /**
   * QNames are not compared with prefix
   */ 
  implicit val defaultAttributeEquals = equal { 
    (a : Attribute, b : Attribute) =>
      defaultAttributeComparison.compare(false, Nil, a, b).isEmpty
  }
}

object AttributeEquals extends DefaultAttributeEquals {}

trait ExactQName{
  /**
   * QNames are compared with prefix
   */ 
  implicit val prefixAttributeComparison = new AttributeComparison()(equal { (a: QName, b: QName) => a.====(b) })
  
  /**
   * QNames are compared with prefix
   */ 
  implicit val prefixAttributeEquals = equal { 
    (a : Attribute, b : Attribute) =>
      prefixAttributeComparison.compare(false, Nil, a, b).isEmpty
  }
}

/**
 * Comparisoms built on exact qname comparisom, prefixes can be important for some systems
 */ 
object ExactQName extends ExactQName {}

/**
 * Comparison for attributes, requires a comparison for individual attributes, allowing flexible definitions of equality
 */ 
class AttributesComparison( implicit ac : XmlComparison[Attribute]) extends XmlComparison[Attributes] {
  def compare( calculate : Boolean, path : BasicPath, left : Attributes, right : Attributes) : Option[(XmlDifference[_], BasicPath)] = {
    import EqualsHelpers._
    import scales.utils.collectFirst

    if (left.size != right.size)
      if (calculate)
	Some((DifferentNumberOfAttributes(left,right), path))
      else
	noCalculation
      else 
	// get the first error
	collectFirst[Attribute, (XmlDifference[_], BasicPath)](left){ 
	  x1 =>
	    right( x1.name ).cata( x2 => {// if we have it check the attributes
	      val r = ac.compare(calculate, path, x1, x2)
	      if (r.isDefined) {
		// it can only be AttributeValueDifference
		if (calculate)
		  Some( (DifferentValueAttributes( left, right, x1 ), path) )
		else
		  noCalculation
	      } else None
	    } , 
	    if (calculate)
	      Some( ( MissingAttributes( left, right, x1 ), path) )
	    else
	      noCalculation     
	    )
	}
  }
}

trait DefaultAttributesEquals {
  implicit val defaultAttributesComparison = new AttributesComparison()( AttributeEquals.defaultAttributeComparison )

  implicit val defaultAttributesEquals = equal {
    (a : Attributes, b : Attributes) =>
      defaultAttributesComparison.compare(false, Nil, a, b).isEmpty
  }
}

object AttributesEquals extends DefaultAttributesEquals {}

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
  def compare( calculate : Boolean, path : BasicPath, left : Elem, right : Elem) : Option[(XmlDifference[_], BasicPath)] = {

    if (!(left.name === right.name))
      if (calculate)
	Some((ElemNameDifference(left,right), path))
      else
	noCalculation
      else 
	ac.compare(calculate, path, left.attributes, right.attributes).
    cata( x => 
      if (calculate) {
	val (attrs : AttributesDifference, p) = x
	Some( (ElemAttributeDifference( left, right, attrs), path) )
      } else noCalculation
    ,
      if (!namespaces.equal(left.namespaces, right.namespaces))
	if (calculate)
	  Some( (ElemNamespacesDifference(left, right), path) )
	else
	  noCalculation
	else None
    )
  }
}


trait DefaultElemEquals {
  implicit val defaultElemComparison = new ElemComparison()( AttributesEquals.defaultAttributesComparison, ScalesXml.qnameEqual)

  implicit val defaultElemEquals = equal {
    (a : Elem, b : Elem) =>
      defaultElemComparison.compare(false, Nil, a, b).isEmpty
  }
}

object ElemEquals extends DefaultElemEquals {}

/**
 * Compares based on streams.  Requires comparisons for XmlItem, Elem and a QName Equal instance for testing EndElems.
 */ 
class StreamComparison( implicit ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], endElemQNameEqual : Equal[QName]) extends XmlComparison[Iterator[PullType]] {
  def compare( calculate : Boolean, opath : BasicPath, lefti : Iterator[PullType], righti : Iterator[PullType] ) : Option[(XmlDifference[_], BasicPath)] = {

    var res : Option[(XmlDifference[_], BasicPath)] = None
    val joined = lefti.zip(righti)
    var path = opath

    while( res.isEmpty && joined.hasNext) {
      joined.next match {
	case (Left(x : Elem), Left(y : Elem)) =>
	  path = startElem(x.name, path)
	  res = ec.compare(calculate, path, x, y)
	case (Left(x : XmlItem), Left(y : XmlItem)) =>
	  res = ic.compare(calculate, path, x, y)
	case (Right(x : EndElem), Right(y  : EndElem)) =>
	  res = 
	    if (!endElemQNameEqual.equal(x.name, y.name)) {
	      if (calculate)
		Some((EndElemNameDifference(x, y), path))
	      else 
		noCalculation
	    } else None
	  path = endElem(path)
	case (left, right) => 
	  res = 
	    if (calculate)
	      Some((DifferentTypes(left, right), path)) 
	    else
	      noCalculation
      }
    }

    res
  }
}

trait DefaultStreamEquals {
  /**
   * Help inference out
   */ 
  implicit def toDefaultStreamComparison[T <: Iterator[PullType]] : XmlComparison[T] = defaultStreamComparison.asInstanceOf[XmlComparison[T]]

  implicit val defaultStreamComparison : XmlComparison[Iterator[PullType]] = new StreamComparison()( ItemEquals.DefaultXmlItemComparison, ElemEquals.defaultElemComparison, ScalesXml.qnameEqual)

  implicit val defaultStreamEquals = equal {
    (a : Iterator[PullType], b : Iterator[PullType]) =>
      defaultStreamComparison.compare(false, Nil, a, b).isEmpty
  }
}

object StreamEquals extends DefaultStreamEquals {}
