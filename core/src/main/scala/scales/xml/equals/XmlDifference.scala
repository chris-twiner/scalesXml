package scales.xml.equals

import scales.xml.{PullType, QName, Elem, Attribs, Attributes, Attribute, XmlItem, XmlPath, EndElem, XCC, Misc, Miscs}

import scales.xml.serializers.NamespaceContext

import scala.collection.immutable.{ Stack, Map }

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
object SomeDifference extends XmlDifference[AnyRef] {
  val left = null
  val right = null

  def unapply(a : XmlDifference[AnyRef]) : Option[(AnyRef, AnyRef)] = Some((left,right))

  val noCalculation : Option[(XmlDifference[_], ComparisonContext)] = Some((this, ComparisonContext()))
}

/**
 * When types are different, or an end element vs XmlEvent
 */ 
case class DifferentTypes( left : PullType, right : PullType ) extends XmlDifference[PullType]

case class QNameDifference( left : QName, right : QName ) extends  XmlDifference[QName]

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
 * Re-packs the AttributeValueDifference, but will only occur when the attributes QName is present
 */
case class DifferentValueAttributes( left : Attributes, right : Attributes, differentValue : Attribute ) extends AttributesDifference

/**
 * The size is the same but one of lefts qnames is not present in right
 */ 
case class MissingAttributes( left : Attributes, right : Attributes, missing : Attribute ) extends AttributesDifference

case class ItemDifference( left : XmlItem, right : XmlItem) extends XmlDifference[XmlItem]

case class ElemNameDifference( left : Elem, right : Elem ) extends ElemDifference

/**
 * If the difference is due to an attribute thats included as well
 */ 
case class ElemAttributeDifference( left : Elem, right : Elem, attributesDifference : AttributesDifference ) extends ElemDifference

/**
 * This can only be returned when an ElemComparisom has been suplied with a custom namespaces implementation.
 */ 
case class ElemNamespacesDifference( left : Elem, right : Elem ) extends ElemDifference

case class EndElemNameDifference( left : EndElem, right : EndElem ) extends XmlDifference[EndElem]

case class MiscDifference( left : Misc, right : Misc, isProlog : Boolean ) extends XmlDifference[Misc]

case class MiscDifferentTypes( left : Misc, right : Misc, isProlog : Boolean ) extends XmlDifference[Misc]

case class DifferentNumberOfMiscs( left : Miscs, right : Miscs, isProlog : Boolean ) extends XmlDifference[Miscs]
