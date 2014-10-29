package scales.xml.equals

import scales.xml.{QName, Elem, Attribs, Attributes, Attribute, XmlItem, Text, PI, CData, Comment, PullType, EndElem, Misc, Miscs, DocLike, impl}

import impl.EqualsHelpers

import XmlEquals._

import scalaz.Equal._
import scalaz._
import Scalaz._

import SomeDifference.noCalculation

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
    if (left eq right)
      None
    else
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
