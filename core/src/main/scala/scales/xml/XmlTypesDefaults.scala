package scales.xml

import scala.collection.immutable.Map

import scala.collection.generic.CanBuildFrom

import impl.EqualsHelpers

//import scales.utils.collection.path.{Path, Node, Position}
import scales.utils.{TreeCBF, subtree, item, top, ItemOrTree, noPath => fno, EitherLike, LeftLike}
import scales.utils.collection.{ListSet, Tree, ImmutableArrayProxy, ImmutableArrayProxyBuilder }
import scales.utils.collection.path.{ Path, Position, Node }

/**
 * Simple constructor for Attributes
 */ 
object Attribs {
  def apply( attribs : Attribute * ) : Attributes = {
    import EqualsHelpers._
    ListSet[Attribute](attribs :_*)
  }
}

trait XmlTypes {

  import EqualsHelpers._

  private[xml] val NotFromParser = scales.xml.impl.NotFromParser

  type XmlCBF = TreeCBF[XmlItem, Elem, XCC]

  type AttributeQName = EitherLike[PrefixedQName, NoNamespaceQName]

  type XmlBuilder = collection.mutable.Builder[ ItemOrElem, XmlChildren ]
  def XmlBuilder() : XmlBuilder = ImmutableArrayProxyBuilder[ItemOrElem]()

  type XmlTree = Tree[XmlItem, Elem, XCC]
  type ItemOrElem = ItemOrTree[XmlItem, Elem, XCC]

  type XmlChildren = XCC[ItemOrElem]

  type XCC[T] = ImmutableArrayProxy[T]

  type Misc = Either[Comment, PI]
  type Miscs = Seq[Misc]

  type Attributes = ListSet[Attribute]

  val emptyMiscs = List[Misc]()

  val emptyAttributes = ListSet.empty[Attribute]
  val emptyNamespaces: Map[String, String] = Map[String, String]()

  val emptyChildren = ImmutableArrayProxy[ItemOrElem]()

  /**
   * Use to signify a "null object", tree/path shouldn't work like this its a smell
   */
  val noXmlPath = fno[XmlItem, Elem, XCC]

  type XmlPath = Path[XmlItem, Elem, XCC]

  /**
   * Will match if the QNames are the same =:= and if the attributes are present (not requiring that these are the only attributes).
   * What is returned is either a Seq of attribute values or a simple boolean
   */
  def ElemMatcher(name: QName, attributes: AttributeQName*) = new {
    import impl.ExtraTypesImplicits._

    def matchAttribs(elem: Elem) = for (attribute <- attributes; matches <- elem.attributes(attribute)) yield matches

    def unapply(elem: Elem): Option[(Elem, List[Attribute])] = {
      val matched = matchAttribs(elem)
      if (elem.name =:= name && matched.size == attributes.size)
        Some((elem, matched.toList))
      else
        None
    }
  }

  final def addAndFocus(path: XmlPath, elem: Elem, dchildren : XmlChildren = emptyChildren) =
    // start it off
    if (path eq noXmlPath) {
      top(Tree(
        elem, dchildren))
    } else {
      import ScalesXml.xmlCBF

      // using a Function1 causes 2% of the cost for init alone
      
      // cache it here as we are going to fudge the path
      val n : ItemOrElem = subtree(elem, dchildren)
      var size = 0

      import path.node.focus
      val parent = focus.getRight
      val c = parent.children
      size = c.length
      
      // add a child, focus on it
      val tpath = Path( path.top, 
		       Node( path.node.index, 
			    Tree(parent.section,
				       c :+ n)) )

      // drill down to last child
      Path(tpath, Node(size, n))
    }

  final def addChild(path: XmlPath, child: XmlItem) = {
    import ScalesXml.xmlCBF
    import path.node.focus

    val parent = focus.getRight
    Path( path.top, 
	       Node( path.node.index,
		     Tree(parent.section,
        parent.children :+ item[XmlItem, Elem, XCC](child))
			       ))
  }

}

