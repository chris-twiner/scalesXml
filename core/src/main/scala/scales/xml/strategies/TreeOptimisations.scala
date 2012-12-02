package scales.xml.strategies

import scales.xml._
import scales.utils.collection.{Tree, ImmutableArrayProxy}

import ImmutableArrayProxy.one

import impl.TreeProxies

/**
 * Allows replacing a tree for memory optimisations
 */ 
trait TreeOptimisation[TOKEN <: scales.xml.OptimisationToken] extends PathOptimisationStrategy[TOKEN] {

  def newTree( elem : Elem, children : XmlChildren, token : TOKEN ) : XmlTree

  final override def elementEnd( xml : TreeProxies, token : TOKEN ) {
    import ScalesXml.xmlCBF

    val l = xml.current
    val nt = newTree(l.elem, l.builder.result, token)
    val d = xml.depth
    val nd = d - 1
    if (d > 0) {
      xml.depth = nd
      val c = xml.proxy( nd )
      xml.current = c
      c.builder.+=(nt)
    } else {
      xml.rootTree = nt 
      xml.depth = nd
    }
  }

}

/**
 * An elem with no attributes, namespaces and only one text value.
 *
 */
abstract class NameValue(val name : QName, val text : String) extends Tree[XmlItem, Elem, XCC] {

  def section : Elem
  def children : XmlChildren = one(Text(text))

  def copy( section : Elem = section, children : XmlChildren = children) : Tree[XmlItem, Elem, XCC] = {
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

  def children : XmlChildren = one(Text(text))

  def copy( section : Elem = section, children : XmlChildren = children) : Tree[XmlItem, Elem, XCC] = {
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
  def apply( section : Elem, children : XmlChildren ) ( implicit fromParser : FromParser ) : XmlTree = 
    if (children.isEmpty)
      Tree(section, children)
    else {
      val head = children.head
      if (head.isLeft) {
	val left = head.getLeft
	if (left.isInstanceOf[Text] && children.size == 1) {
	  if (section.attributes.isEmpty && section.namespaces.isEmpty)
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

/**
 * Optimises QNames and Trees according to LazyOptimisedTree
 */ 
object QNameTreeOptimisation extends TreeOptimisation[QNameToken] with QNameOptimisationT[QNameToken] with QNameTokenF {

  def newTree( elem : Elem, children : XmlChildren, token : QNameToken ) : XmlTree =
    LazyOptimisedTree( elem, children )(IsFromParser)

}

/**
 * Optimised QNames, Elems and Tree, heavily reduced memory consumption with LazyOptimisedTree.
 */
object QNameElemTreeOptimisation extends PathOptimisationStrategy[ElemToken] with ElemQNameOptimisationT[ElemToken] with TreeOptimisation[ElemToken] with ElemTokenF {
  
  def newTree( elem : Elem, children : XmlChildren, token : ElemToken ) : XmlTree =
    LazyOptimisedTree( elem, children )(IsFromParser)

}
