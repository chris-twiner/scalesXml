package scales.xml.strategies

import scales.xml._
import scales.utils._


/**
 * An elem with no attributes, namespaces and only one text value.
 *
 */
abstract class NameValue(val name : QName, val text : String) extends Tree[XmlItem, Elem, XCC] {

  def section : Elem
  def children : XmlChildren

  private[this] def csection = section
  private[this] def cchildren = children

  def copy( section : Elem = csection, children : XmlChildren = cchildren) : Tree[XmlItem, Elem, XCC] = {
    // if we are copying we are no longer in a parse
    import ScalesXml.fromParserDefault
    // TODO go through a list of known optimisations
    NameValue(section, children).getOrElse( 
      Tree(section, children) )
  }
}

object NameValue {

  /**
   * Returns Some(NameValue) if its appropriate
   */ 
  def apply( section : Elem, children : XmlChildren ) ( implicit fromParser : FromParser ) : Option[NameValue] =
    if (section.attributes.isEmpty && section.namespaces.isEmpty && children.size == 1) {
      val head = children.head
      if (head.isLeft) {
	val left = head.getLeft
	if (left.isInstanceOf[Text]) {
	  Some(apply(section.name, left.value))
	} else None
      } else None
    }
    else None

  def apply( iname : QName, itext : String ) ( implicit fromParser : FromParser ) : NameValue = 
    if (fromParser eq NotFromParser)
      new NameValue(iname, itext) {
	
	def section : Elem = Elem(name)(NotFromParser) // have to recheck that the qname is ok
	def children : XmlChildren = IAOne(Text(text))

      }
    else
      new NameValue(iname, itext) {
	
	def section : Elem = Elem(name)(IsFromParser) // don't have to recheck
	def children : XmlChildren = IAOne(Text(text))

      }

}
