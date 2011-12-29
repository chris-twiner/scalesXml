package scales.xml

import scales.utils._
import scala.collection.generic.CanBuildFrom
import scales.utils.one

/** Simple container for keeping relationship between the parent and attribute */
case class AttributePath(attribute: Attribute, parent: XmlPath)

case class XmlPathComparisoms(path: XmlPath) {
  def ===(other: String) = Elements.Functions.text(path) == other
}

case class AttributePathComparisoms(path: AttributePath) {
  def ===(other: String) = Attributes.Functions.text(path) == other
}

/** Attributes that have been selected */
case class AttributePaths[PT <: Iterable[XmlPath]](attributes: Iterable[AttributePath], path: XPathInfo, cbf: CanBuildFrom[PT, XmlPath, PT]) {
  import ScalesXml._
  /** Parents of the attributepaths */
  def \^(): XPath[PT] = new XPath[PT](path.copy(nodes = List(attributes.map(_.parent))), cbf)

  /**
   * Some if there is one, else none
   */
  def one: Iterable[AttributePath] = if (attributes.size != 1) Nil else scales.utils.one(attributes.head)

  /**
   * Some if there is one, else calls handler (allows decisions based on more than one etc.
   */
  def oneOr(handler: Iterable[AttributePath] => Iterable[AttributePath]) = if (attributes.size == 1) scales.utils.one(attributes.head) else handler(attributes)

}

/**
 * XPaths need sorting upon unions etc, and duplicate filtering on \^ parents
 */
case class XPathInfo(nodes: Iterable[Iterable[XmlPath]], mustBeSorted: Boolean = false, filterDuplicates: Boolean = false, initialNode: Boolean = false, eager : Boolean = false, direct : Boolean = false)

import xpath._

/**
 * In contrast to the specs reverse axis are not supported.  This puts
 * an extra effort on the user for translating, but I don't agree with
 * the design anyway.  It adds both ambiguity for an embedded dsl:
 *
 * //a:ShouldRedeclare/../text()[5]/preceding-sibling::text()[1]
 *
 * is in reverse order (above is previousSibling in Path), whereas
 *
 * (//a:ShouldRedeclare/../text()[5]/preceding-sibling::text())[1]
 *
 * is in doc order (the first text child of the parent).  Such an arbritary decision does nothing but irritate.  A simpler design as found in this implementation is that the user can simply use full scala data functions to reverse as needed.
 *
 * (Temptation is to provide the base type as SeqLike instead of Iterable as it should be in a sequence with a defined order, also then providing .reverse and friends)
 */
class XPath[PT <: Iterable[XmlPath]](val path: XPathInfo, val cbf: CanBuildFrom[PT, XmlPath, PT]) extends ElementStep with OtherNodeTypes with AttributeAxis with SiblingsAxis with DocumentSplitters {

  type T = PT

  def newThis(xpathInfo: XPathInfo): XPath[T] =
    new XPath[T](xpathInfo, cbf)

  def empty: Iterable[XmlPath] = cbf().result
  def just(only: XmlPath): Iterable[XmlPath] = (cbf() += only).result

  /**
   * Allow working within the XPath, easily extend and test
   */
  def in(f: (XPath[T]) => XPath[T]): XPath[T] =
    f(this)

  /**
   * Allow working within the XPath, easily extend and test
   */
  def |>(f: (XPath[T]) => XPath[T]) = in(f)

  /**
   * Some when its only one in the result set
   */
  def one: Iterable[XmlPath] = {
    val nodes = ScalesXml.fromXPathToIterable(this)
    if (nodes.size != 1) Nil
    else scales.utils.one(nodes.head)
  }

  def oneOr(handler: XPath[T] => Iterable[XmlPath]) = {
    val nodes = ScalesXml.fromXPathToIterable(this)
    if (nodes.size == 1) scales.utils.one(nodes.head)
    else handler(this)
  }

  /**
   * Union of the two paths, they must use the same type however
   *
   * x | x === x but only when apply fromXPathToIterable (default behaviour anyway)
   *
   * For attribute paths only ++ on an iterable is supported.
   */
  def |( other : XPath[T] ) : XPath[T] = 
    new XPath(XPathInfo( 
      nodes = path.nodes ++ other.path.nodes, 
      mustBeSorted = true, filterDuplicates = true, initialNode = false, eager = path.eager | other.path.eager), cbf)
  
  // TODO figure out unions of other paths (namespaces / attributes)
}

/**
 * Pos and filter for a direct access does not flatmap.  The next / breaks the chain
 */
class DirectXPath[PT <: Iterable[XmlPath]](override val path: XPathInfo, override val cbf: CanBuildFrom[PT, XmlPath, PT]) extends XPath[PT](path, cbf) {
  override def process(newNodes: Iterable[Iterable[XmlPath]],  info : XPathInfo = path) = {
      newThis(info.copy( nodes = newNodes ))
    }
}

object Axis {

  /**
   * eager recursive unpack, better performance when its eager
   */ 
  def eager_recUnpack(xmlPath: Iterable[XmlPath]): List[XmlPath] = {
    var res : List[XmlPath] = Nil

    val i = xmlPath.iterator
    while(i.hasNext) {
      val child = i.next
      if (child.isItem) 
	res = child :: res
      else {
	// child first then its kids
	res = (child :: res ).reverse_::: (
	  eager_recUnpack(child: Iterable[XmlPath]) )
      }
    }
    res
  }

  /**
   * Possibly lazy recursive unpack, used by l_\\ and descendant
   */ 
  def lazy_recUnpack(axis : Axis)(xmlPath: Iterable[XmlPath]): Iterable[XmlPath] =
    xmlPath.flatMap { child =>
      if (child.isItem) axis.just(child) 
      else {
	// child first then its kids
	axis.just(child) ++
	lazy_recUnpack(axis)(child: Iterable[XmlPath])
      }		       
    }

}

/**
 * Base trait for XPath Axis, also provides common functionality
 */ 
  trait Axis {
    import Axis._

    type T <: Iterable[XmlPath]
    val cbf: CanBuildFrom[T, XmlPath, T]

    val path: XPathInfo

    def newThis(xpathInfo: XPathInfo): XPath[T]

    def process(newNodes: Iterable[Iterable[XmlPath]], info : XPathInfo = path) = {
      val nn =
	if (path.eager) {
	  if (newNodes.exists(_.isEmpty)) // look first
	    newNodes.filter(!_.isEmpty) 
	  else
	    newNodes
	} else newNodes.filter(!_.isEmpty) // do it anyway

      newThis(info.copy( nodes = nn))
    }

    def empty: Iterable[XmlPath]
    def just(only: XmlPath): Iterable[XmlPath]

    /**
     * x prefixed to avoid disturbing the use of an xpath as an iterable
     */ 
    final def xflatMap(f : Iterable[XmlPath] => Iterable[Iterable[XmlPath]] ) =
      process( path.nodes.flatMap( f ) )

    final def xfilter( f : Iterable[XmlPath] => Boolean ) = 
      process(path.nodes.filter( f ) )

    protected final def xlast( onN : Int => Boolean ) : XPath[T] =
      xfilter{ x => onN( x.size) }

    /**
     * Equivalent to [last() > n]
     */ 
    def last_>(n : => Int): XPath[T] =
      xlast( _ > n )

    /**
     * Equivalent to [last() < n]
     */ 
    def last_<(n : => Int): XPath[T] =
      xlast( _ < n )

    /**
     * Equivalent to [last() = n]
     */ 
    def last_==(n : => Int): XPath[T] =
      xlast( _ == n )

    /**
     * Equivalent to position() = last()
     */ 
    def pos_eq_last : XPath[T] =
      xmap { _.lastOption }

    /**
     * Equivalent to [position() > pos]
     */ 
    def pos_>(pos : => Int): XPath[T] =
      xflatMap { path =>
	val n = path.drop(pos)
	if (n.isEmpty)
	  empty
	else
	  one(n)
      }

    /**
     * Equivalent to [position() < pos]
     */ 
    def pos_<(pos : => Int): XPath[T] =
      xflatMap { path =>
	val n = path.take(pos-1)
        if (n.isEmpty)
	  empty
	else
	  one(n)
      }
    
    def pos_==(p: => Int) = pos(p)

    /**
     * 1 index based, per spec, but unsure if it should be
     */
    def pos(pos: => Int): XPath[T] = 
      xflatMap { path =>
	val n = path.take(pos)
		 
	if (n.size != pos) 
	  empty
	else 
	  one(just(n.last))               
      }

    /** Parents of these paths */
    def \^ : XPath[T] =
      /* must process dups
       * flatten, only each elem should get through anyway
       */
      process(path.nodes.flatMap { path =>
	path.flatMap { x =>
          if (x.top.isLeft) empty
          else one(just(x.top.right.get))
        }
      }, path.copy(initialNode = false, direct = false))

    /** simply forwards the current context, element\item specific versions exist that step downwards in document order */
    def \ : XPath[T] =
      newThis(path.copy(initialNode = false, direct = false))

    /**
     * all children unpacked, normal xpath requires the context in E1/E2, which hugely complicates things, so \ is this, \+ is \ in the case where unpacking is needed
     * which just leaves \* varieties for elements.
     */
    def \+ : XPath[T] =
      process(path.nodes.flatMap {
        _.flatMap { child =>
          if (child.isItem) just(child)
          else one(child) //: Iterable[XmlPath] 
        }
      }, path.copy(initialNode = false, direct = false))

    /**
     * All descendants, uses XPathInfo eager to choose an implementation
     */ 
    def \\ : XPath[T] = {
      val f = 
	if (path.eager) eager_recUnpack _
	else lazy_recUnpack(this) _

      process(
        (if (path.initialNode) path.nodes // crazy initial expression thing 3.2 in http://www.w3.org/TR/xpath20/#id-path-expressions
        else List())
          ++ path.nodes.flatMap { f },
	 path.copy(initialNode = false, direct = false))
    }

    
    final def xmap( f : Iterable[XmlPath] => Iterable[XmlPath] ) =
      process(path.nodes.map{ f })

    /** filter through the current matches */
    final def filter(pred: XmlPath => Boolean): XPath[T] =
      xmap{ _.filter(pred) }

  }
