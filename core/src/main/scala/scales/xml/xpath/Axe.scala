package scales.xml.xpath

import scales.xml._

import scales.utils._

/**
 * Unlike XPath spec no reverse axis are directly provided by the dsl
 */
trait SiblingsAxis extends Axis {

  /**
   * All siblings before the context
   */
  def preceding_sibling_:: : XPath[T] =
    xflatMap {
      _.filter { _.hasPreviousSibling }.
        map {
          path =>
            // each preceding
            val pos = path.node.index

            path.zipUp.splitAt(pos)._1.toList.reverse
        }
    }

  /**
   * All siblings after the context
   */ 
  def following_sibling_:: : XPath[T] =
    xflatMap {
      _.filter { _.hasNextSibling }.
        map {
          path =>
            val pos = path.node.index

            path.zipUp.splitAt(pos)._2.drop(1)
        }
    }

}

/** The * and @ must be swapped otherwise its an annotation */
trait AttributeAxis extends Axis {

  def i_*@ : Iterable[AttributePath] =
    path.nodes.flatMap {
      _.flatMap { child => // we are looking only for immediate attribs, not kids
        if (child.isItem == true) Nil
        else child.tree.section.attributes.map { x =>
          AttributePath(x, child);
        }
      }
    }

  import ScalesXml._

  /** All immediate attributes */
  def *@ : AttributePaths[T] =
    AttributePaths(i_*@, path, cbf)

  /** Search for all immediate attributes with a matching predicate (allows localName only searches for example) */
  def *@(pred : AttributePath => Boolean) : AttributePaths[T] =
    AttributePaths(i_*@.filter { pred(_) }, path, cbf)

  /** Special case for AttributeQNames, only local and namespace match */
  def *@(attrQName : AttributeQName) : AttributePaths[T] =
    *@({ attributePath : AttributePath => toQName(attributePath.attribute.name) =:= toQName(attrQName) })

  /** Loose match for UnprefixedQNames only, only matches local and namespace */
  def *@(qname : UnprefixedQName) : AttributePaths[T] =
    *@({ attributePath : AttributePath => toQName(attributePath.attribute.name) =:= qname })

  def \@ = \.*@

  def \@(pred : AttributePath => Boolean) : AttributePaths[T] =
    \.*@(pred)

  /** Special case for AttributeQNames, only local and namespace match */
  def \@(attrQName : AttributeQName) : AttributePaths[T] =
    \.*@(attrQName)

  /** Loose match for UnprefixedQNames only, only matches local and namespace */
  def \@(qname : UnprefixedQName) : AttributePaths[T] =
    \.*@(qname)

  def \\@ = \\.*@

  def \\@(pred : AttributePath => Boolean) : AttributePaths[T] =
    \\.*@(pred)

  /** Special case for AttributeQNames, only local and namespace match */
  def \\@(attrQName : AttributeQName) : AttributePaths[T] =
    \\.*@(attrQName)

  /** Loose match for UnprefixedQNames only, only matches local and namespace */
  def \\@(qname : UnprefixedQName) : AttributePaths[T] =
    \\.*@(qname)

}

trait ElementStep extends Axis {

  /** all element children */
  private[this] def \*- : XPath[T] =
    xflatMap(_.filter { isElem })

  /** Search for all immediate child elements with a matching qname */
  def *(qname : QName) : XPath[T] = filter(x => x.isItem == false && qname =:= x.tree.section.name)

  def \*(qname : QName) : XPath[T] =
    \*-.*(qname)

  /** Search for all immediate child elements matching the predicate*/
  def *(pred : XmlPath => Boolean) : XPath[T] = filter(x => x.isItem == false && pred(x))

  def \*(pred : XmlPath => Boolean) : XPath[T] = \*-.*(pred)

  /** Immediate child with position pos, should only be an element. NOTE this will always be evaluated in place and is 1 indexed */
  def *(pos : Int) : XPath[T] =
    xflatMap { path =>
      val n = path.filter(_.isItem == false).take(pos)
      if (n.size != pos) empty
      else one(just(n.last))
    }

  def \*(pos : Int) : XPath[T] = \*-.*(pos)

  /** all child elements */
  def * : XPath[T] =
    filter(isElem)
  //process(path.nodes.flatMap{ _.filter(_.isItem == false) })

  def \* : XPath[T] = \*-.*

  def \\* : XPath[T] = \\.*
  
  def \\*(qname : QName) = \\.*(qname)
  
  def \\*(pred : XmlPath => Boolean ) = \\.*(pred)
  
  def \\*(pos : Int) = \\.*(pos)
}

/**
 * Little depature from standard, rather than duplicate all of the \ and \\ nodes, just a simple \\*() comment() will do
 *
 * NOTE these have a large performance hit as the must sort and filter the results in order to get doc order.  Better to use specific nodes
 * avoiding \\ etc, then use the s for simple functions, sutext etc for simple useful.  Additionally using the non simple versions incurs the cost
 * of adding adjacent text node processing as per standard datamodel (all adjacent text nodes are joined and any resulting nodes that are "the zero-length string" '' are removed).
 *
 * TODO xpath adjacent text node handling
 *
 * @author Chris
 *
 */
trait OtherNodeTypes extends Axis {
  /** all text child elements - not including cdata nodes, neither sorted nor dup filtered */
  def textOnly : XPath[T] =
    filter(x => x.isItem == true && x.item().isInstanceOf[Text])

  /** all text child elements - xpath standard way, including cdata nodes, filtered to join adjacent text nodes */
  def text : XPath[T] =
    filter(isText)

  /** all cdata child elements */
  def cdata : XPath[T] =
    filter(x => x.isItem == true && x.item().isInstanceOf[CData])

  /** all commnent child elements */
  def comment : XPath[T] =
    filter(x => x.isItem == true && x.item().isInstanceOf[Comment])

  /** all pi child elements */
  def pi : XPath[T] =
    filter(x => x.isItem == true && x.item().isInstanceOf[PI])
}

/**
 * Following and preceding split the document in two
 */ 
trait DocumentSplitters extends Axis {

  import Axis._

  /**
   * As per spec all children of a context but predicates work directly on these children.
   * i.e. path.\.descendant_::.*(pred).*(1) returns the first elem for which pred is true whereas
   * path.\\*(pred).*(1) returns ALL the first elems for which this is true.
   *
   * 
   */ 
  def descendant_:: : XPath[T] = {
    val f = lazy_recUnpack(this) _
    // eager can't work as we can only evaluate one step at a time
//      if (path.eager) pe_recUnpack _
//      else pl_recUnpack _

    process(path.nodes.map { f }, path.copy(direct = true))
  }

  /**
   * All nodes following this given node, in document order, excluding descendants
   */ 
  def following_:: : XPath[T] = 
    xflatMap { _.map{
      new Following( _ )      
    } }
  
  /**
   * All nodes preceding this given context, in reverse document order, excluding parents 
   */
  def preceding_:: : XPath[T] =
    xflatMap { _.map{
      new Preceding( _ )
    } }

  /**
   * All parents of this node
   */ 
  def ancestor_:: : XPath[T] =
    xflatMap { _.map{
      new Ancestors( _ )      
    } }

  /**
   * All parents of this node or this node
   */ 
  def ancestor_or_self_:: : XPath[T] =
    xflatMap { _.map{ x =>
      new Ancestors( x ) ++ one(x)
    } }

  /**
   * All descendants of this node or self
   */ 
  def descendant_or_self_:: : XPath[T] = {
    val f = lazy_recUnpack(this) _
    process(path.nodes.map { x => f(x) ++ x }, 
	    path.copy(direct = true))
  }

}
