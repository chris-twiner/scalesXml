package scales.xml.impl

import scala.collection.immutable.Map

//import scales.utils.collection.path.{Path, Node, Position}
import scales.utils.collection.path.{Node, Path}
import scales.utils.collection.{ImmutableArrayProxy, ImmutableArrayProxyBuilder, Tree}
import scales.utils.{EitherLike, ItemOrTree, TreeCBF, item, subtree, top, noPath => fno}
import scales.xml.{Attribute, Comment, Elem, NoNamespaceQName, PI, PrefixedQName, QName, ScalesXml, XmlItem}

trait XmlTypes {

  import EqualsHelpers._

  /**
   * Alias for the XmlTree CanBuildFrom
   */ 
  type XmlCBF = TreeCBF[XmlItem, Elem, XCC]

  /**
   * An AttributeQName is either a PrefixedQName or a NoNamespaceQName
   */ 
  type AttributeQName = EitherLike[PrefixedQName, NoNamespaceQName]

  /**
   * Alias for a mutable builder of XmlChildren
   */ 
  type XmlBuilder = collection.mutable.Builder[ ItemOrElem, XmlChildren ]
  /**
   * Default implementation for constructing an instance of XmlBuilder (ImmutableArrayProxyBuilder)
   */ 
  def XmlBuilder() : XmlBuilder = ImmutableArrayProxyBuilder[ItemOrElem]()

  /**
   * Alias for Trees of Elem and XmlItem
   */ 
  type XmlTree = Tree[XmlItem, Elem, XCC]

  /**
   * Alias for An XML ItemOrTree
   */ 
  type ItemOrElem = ItemOrTree[XmlItem, Elem, XCC]

  /**
   * An Alias for a collection of ItemOrElem, the children of a given tree node
   */ 
  type XmlChildren = XCC[ItemOrElem]

  /**
   * XML Collection - an alias for ImmutableArrayProxy 
   */ 
  type XCC[T] = ImmutableArrayProxy[T]

  /**
   * Misc is either a Comment or PI, and is used for the Prolog and trailing Misc items in a Doc.
   */ 
  type Misc = Either[Comment, PI]
  /** A collection of Misc */
  type Miscs = Seq[Misc]

  /**
   * An alias for am immutable ListSet of Attribute 
   */ 
  //type Attributes = ListSet[Attribute]
  type Attributes = scales.utils.collection.ArraySet[Attribute]

  /** An empty collection of Misc */ 
  val emptyMiscs: Miscs = List[Misc]()

  /** An empty collection of Attribute */
//  val emptyAttributes: Attributes = ListSet.empty[Attribute]
  val emptyAttributes: Attributes = AttributeSet.empty//ListSet.empty[Attribute]
  /** An empty Map of String -> String representing prefix -> namespace */ 
  val emptyNamespaces: Map[String, String] = Map[String, String]()

  /** An empty collection of ItemOrElem */ 
  val emptyChildren: XmlChildren = ImmutableArrayProxy[ItemOrElem]()

  /**
   * Use to signify a "null object", tree/path shouldn't work like this its a smell
   */
  val noXmlPath: XmlPath = fno[XmlItem, Elem, XCC]

  /**
   * An alias for a Path over XmlTree
   */ 
  type XmlPath = Path[XmlItem, Elem, XCC]

  /**
   * Will match if the QNames are the same =:= and if the attributes are present (not requiring that these are the only attributes).
   * What is returned is either a Seq of attribute values or a simple boolean
   */
  def ElemMatcher(name: QName, attributes: AttributeQName*) = new {

    def matchAttribs(elem: Elem) = for (attribute <- attributes; matches <- elem.attributes(attribute)) yield matches

    def unapply(elem: Elem): Option[(Elem, List[Attribute])] = {
      val matched = matchAttribs(elem)
      if (elem.name =:= name && matched.size == attributes.size)
        Some((elem, matched.toList))
      else
        None
    }
  }

  /**
   * Adds a subtree to this given path and returns a path focussed on the new subtree.
   * 
   * The Tree is constructed from the elem and optional children.
   */
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

  /**
   * Adds a child to the given subpath, however focus remains on the newly modified path
   */ 
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

