package scales.xml.strategies

import scales.xml._
import scales.utils._

/**
 * Allows replacing a tree for memory optimisations
 */ 
trait TreeOptimisation[TOKEN <: scales.xml.OptimisationToken] extends PathOptimisationStrategy[TOKEN] {

  def newTree( elem : Elem, children : XmlChildren, token : TOKEN ) : XmlTree

  final override def elementEnd( xml : TreeProxies, token : TOKEN ) {

    import scales.utils.Tree
    import ScalesXml.xmlCBF

    val l = xml.current
    if (xml.depth > 0) {
      xml.depth -= 1
      xml.current = xml.proxies( xml.depth )
      xml.current.children = (xml.current.children :+ 
	newTree(l.elem, l.children, token)        
      )
    } else {
      xml.depth -= 1
    }

  }

}

/**
 * An elem with no attributes, namespaces and only one text value.
 *
 */
abstract class NameValue(val name : QName, val text : String) extends Tree[XmlItem, Elem, XCC] {

  def section : Elem
  def children : XmlChildren = IAOne(Text(text))

  private[this] def csection = section
  private[this] def cchildren = children

  def copy( section : Elem = csection, children : XmlChildren = cchildren) : Tree[XmlItem, Elem, XCC] = {
    // if we are copying we are no longer in a parse
    import ScalesXml.fromParserDefault
    LazyOptimisedTree(section, children)
  }
}

/**
 * An elem with attributes or namespaces and only one text value.
 *
 */
class ElemValue(val section : Elem, val text : String) extends Tree[XmlItem, Elem, XCC] {

  def children : XmlChildren = IAOne(Text(text))

  private[this] def csection = section
  private[this] def cchildren = children

  def copy( section : Elem = csection, children : XmlChildren = cchildren) : Tree[XmlItem, Elem, XCC] = {
    // if we are copying we are no longer in a parse
    import ScalesXml.fromParserDefault
    LazyOptimisedTree(section, children)
  }
}


/**
 * Collection of optimisations that reduce memory significantly at the cost of parsing performance, and later non-lazy creation of correct objects for accessing the tree.
 *
 * This can have confusing runtime evaluation of XPaths etc.  These optimisations are appropriate for trees that are seldom read, repeated reads may generate too much garbage.
 */ 
object LazyOptimisedTree {

  /**
   * Returns an optimised Tree if Possible
   */ 
  def apply( section : Elem, children : XmlChildren ) ( implicit fromParser : FromParser ) : XmlTree = {
    val head = children.head
    if (head.isLeft) {
      val left = head.getLeft
      if (left.isInstanceOf[Text]) {
	if (section.attributes.isEmpty && section.namespaces.isEmpty && children.size == 1)
	  newNameValue(section.name, left.value)
	else
	  new ElemValue(section, left.value)
      } else Tree(section, children)
    } else Tree(section, children)
  }

  def newNameValue( iname : QName, itext : String ) ( implicit fromParser : FromParser ) : NameValue = 
    if (fromParser eq NotFromParser)
      new NameValue(iname, itext) {
	
	def section : Elem = Elem(name)(NotFromParser) // have to recheck that the qname is ok

      }
    else
      new NameValue(iname, itext) {
	
	def section : Elem = Elem(name)(IsFromParser) // don't have to recheck

      }

}
