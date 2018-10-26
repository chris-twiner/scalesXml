package scales.xml.impl

import scalaz.Equal
import scalaz.Equal._

/**
 * Needed to help with cyclic issues in multi threaded MODULE$ access, after startup no problem, but we can't gaurantee that.
 */ 
object EqualsHelpers extends scales.xml.equals.DefaultQNameEquals {
  
  import scales.xml.{Attribute, AttributeQName, QName}
  
  // only used for default attribute comparisoms in Attributes
  implicit val aqnameEqual: Equal[Attribute] = equal { (a: Attribute, b: Attribute) => a.name =:= b.name }
  implicit val qnameEquiv: scales.utils.Equiv[QName] = new scales.utils.Equiv[QName]
  implicit def toQName(attribQName: AttributeQName): QName = attribQName.asInstanceOf[QName] // otherwise it blows the stack calling itself
  implicit val toQNameF: Attribute => QName = (a: Attribute) => { a.name : QName }
}
