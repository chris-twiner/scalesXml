package scales.xml.equals

import scales.xml._

import scalaz._
import Scalaz._

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
}

/**
 * When types are different, or an end element vs XmlEvent
 */ 
case class DifferentTypes( left : PullType, right : PullType ) extends XmlDifference[PullType]

sealed trait AttributeDifference extends XmlDifference[Attribute]
sealed trait ElemDifference extends XmlDifference[Elem]

case class AttributeNameDifference( left : Attribute, right : Attribute ) extends AttributeDifference
case class AttributeValueDifference( left : Attribute, right : Attribute ) extends AttributeDifference

case class ItemDifference( left : XmlItem, right : XmlItem) extends XmlDifference[XmlItem]

case class ElemNameDifference( left : Elem, right : Elem ) extends ElemDifference

/**
 * If the difference is due to an attribute thats included as well
 */ 
case class ElemAttributeDifference( left : Elem, right : Elem, attributeDifference : AttributeDifference ) extends ElemDifference

object BasicPaths {
  type BasicPath = List[(QName, Map[QName, Int])] 
}

import BasicPaths._

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
	  else Some((
	    if (calculate)
	      ItemDifference(left, right)
	    else
	      SomeDifference
	      , path))

      (left, right) match { // we have to do it on types as well
	case (Text(valu), Text(value)) => check(valu, value)
	case (Comment(com), Comment(comm)) => check(com, comm)
	case (CData(cd), CData(cda)) => check(cd, cda)
	case (PI(ta, valu), PI(tar, value)) => 
	  check(ta, tar) orElse check(valu,  value)
	case _ => Some((
	  if (calculate)
	    DifferentTypes(left, right)
	  else SomeDifference, path))
      }
    }
  }
  
  implicit val defaultXmlItem : Equal[XmlItem] = equal {
    (a : XmlItem, b : XmlItem) =>
      DefaultXmlItemComparisom.compare(false, Nil, a, b).isEmpty 
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
