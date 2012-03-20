package scales.xml.equals

import scales.xml._

import scalaz._
import Scalaz._

/**
 * Where did it fail
 */ 
object BasicPaths {
  type BasicPath = List[(QName, Map[QName, Int])] 
}

import BasicPaths._

/**
 * Why did equality fail, pattern match fun
 */ 
sealed trait XmlDifference[X] {
  val left : X
  val right : X
}

/**
 * Magik object for when we aren't attempting to calculate whats wrong 
 */ 
protected[xml] object SomeDifference extends XmlDifference[AnyRef] {
  val left = null
  val right = null

  def unapply(a : XmlDifference[AnyRef]) : Option[(AnyRef, AnyRef)] = Some((left,right))

  val noCalculation : Option[(XmlDifference[_], BasicPath)] = Some((this, Nil))
}

import SomeDifference.noCalculation

/**
 * When types are different, or an end element vs XmlEvent
 */ 
case class DifferentTypes( left : PullType, right : PullType ) extends XmlDifference[PullType]

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
 * The size is the same but one of lefts qnames is not present in right
 */ 
case class MissingAttributes( left : Attributes, right : Attributes, missing : Attribute ) extends AttributesDifference

case class ItemDifference( left : XmlItem, right : XmlItem) extends XmlDifference[XmlItem]

case class ElemNameDifference( left : Elem, right : Elem ) extends ElemDifference

/**
 * If the difference is due to an attribute thats included as well
 */ 
case class ElemAttributeDifference( left : Elem, right : Elem, attributeDifference : AttributeDifference ) extends ElemDifference

/**
 * Like Equals but also gives a path in addition to the fun reason
 * 
 */ 
trait XmlComparisom[T] {
  /**
   * Takes the path for information reasons (works for streams as well).  The return is either None == boolean or the reason
   */ 
  def compare( calculate : Boolean , path : BasicPath, left : T, right : T) : Option[(XmlDifference[_], BasicPath)]
}

object ItemEquals {

  implicit object DefaultXmlItemComparisom extends XmlComparisom[XmlItem] {
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
      DefaultXmlItemComparisom.compare(false, Nil, a, b).isEmpty 
  }
}

object AttributeEquals {
  class AttributeComparisom(implicit eqn : Equal[QName]) extends XmlComparisom[Attribute] {
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

  /**
   * QNames are not compared with prefix
   */ 
  implicit val defaultAttributeComparisom = new AttributeComparisom()(ScalesXml.qnameEqual)
  
  /**
   * QNames are not compared with prefix
   */ 
  implicit val defaultAttributeEquals = equal { 
    (a : Attribute, b : Attribute) =>
      defaultAttributeComparisom.compare(false, Nil, a, b).isEmpty
  }

  /**
   * Comparisoms built on exact qname comparisom, prefixes can be important for some systems
   */ 
  object ExactQName {
    /**
     * QNames are compared with prefix
     */ 
    implicit val prefixAttributeComparisom = new AttributeComparisom()(equal { (a: QName, b: QName) => a.====(b) })
    
    /**
     * QNames are compared with prefix
     */ 
    implicit val prefixAttributeEquals = equal { 
      (a : Attribute, b : Attribute) =>
	prefixAttributeComparisom.compare(false, Nil, a, b).isEmpty
    }
  }
}

object AttributesEquals {

  class AttributesComparisom( implicit ac : XmlComparisom[Attribute]) extends XmlComparisom[Attributes] {
    def compare( calculate : Boolean, path : BasicPath, left : Attributes, right : Attributes) : Option[(XmlDifference[_], BasicPath)] = {
      import EqualsHelpers._

      if (left.size != right.size)
	if (calculate)
	  Some((DifferentNumberOfAttributes(left,right), path))
	else
	  noCalculation
      else 
	// get the first error
	scales.utils.collectFirst[Attribute, (XmlDifference[_], BasicPath)](left){ 
	  x1 =>
	    right( x1.name ).flatMap{ x2 => // if we have it check the attributes
	      ac.compare(calculate, path, x1, x2)
	    }.orElse{ // right( x1. name ) is none
	      if (calculate)
		Some( ( MissingAttributes( left, right, x1 ), path) )
	      else
		noCalculation	      
	    }
	}
    }
  }

  implicit val defaultAttributesComparisom = new AttributesComparisom()( AttributeEquals.defaultAttributeComparisom )

  implicit val defaultAttributesEquals = equal {
    (a : Attributes, b : Attributes) =>
      defaultAttributesComparisom.compare(false, Nil, a, b).isEmpty
  }
}

/**
 * Class to create importable implicits for xml comparisom
 
class XmlEqualImplicits( implicit startQnameEqual : Equal[QName], startItemEqual : Equal[XmlItem] ) {
  implicit val qnameEqual : Equal[QName] = startQnameEqual
  implicit val itemEqual : Equal[XmlItem] = startItemEqual
  implicit val attributesEqual : Equal[Attributes] = equal {
    (a : Attributes, a2 : Attributes) =>
      (if (a.size == a2.size)
	true
       else {
	 println("counts were different "+a.size+" " + a2.size)
	 false
       }
     ) && 
    a.forall{ x1 => 
      a2( x1.name ).map{ x2 => 
    	if (x1.value == x2.value) true
	else {
	  println("Attribute ("+ x1.name +", "+ x1.value +") has a different value "+ x2.value)
	  false
	} }.getOrElse{
    	  println("missing attribute "+ x1.name)
    	  false}
	   }
  }
    
}*/ 
