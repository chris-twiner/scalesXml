package scales.xml.equals

import scalaz.Equal
import scalaz.Equal._
import scalaz.syntax.equal._
import scalaz.syntax.std.option._
import scales.xml.equals.SomeDifference.noCalculation
import scales.xml.equals.XmlEquals._
import scales.xml.impl.EqualsHelpers
import scales.xml.{Attribute, Attributes, CData, Comment, PI, QName, Text, XmlItem, impl}

/**
 * Like Equals but also gives a path in addition to the fun reason
 * 
 */ 
trait XmlComparison[-T] {

  /**
   * Takes the context for information reasons (works for streams as well).  The return is either None == boolean or the reason
   */ 
  def compare( calculate : Boolean , context : ComparisonContext, left : T, right : T) : Option[(XmlDifference[_], ComparisonContext)]
}

trait DefaultQNameEquals {
  implicit val qnameEqual : Equal[QName] = equal { (a: QName, b: QName) => a =:= b }
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

object DefaultQNameToken extends DefaultQNameToken {
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

    if (left eq right)
      None // worth checking
    else (left, right) match { // we have to do it on types as well
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
    if (left eq right)
      None
    else if (!eqn.equal(toQName(left.name), toQName(right.name)))
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

    if (left eq right)
      None
    else
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

