package scales.xml

import scala.collection.immutable.Map

import scala.collection.generic.CanBuildFrom

import scalaz._
import Scalaz._

import scales.utils.{ListSet, MapSet, Key, ImmutableArray, EitherLike, LeftLike, Tree}

import java.nio.charset.Charset

/**
 * Needed to help with cyclic issues in multi threaded MODULE$ access, after startup no problem, but we can't gaurantee that.
 */ 
object EqualsHelpers {
  implicit val aqnameEqual = equal { (a: Attribute, b: Attribute) => a.name =:= b.name }
  implicit val qnameEqual = equal { (a: QName, b: QName) => a =:= b }
  implicit val qnameEquiv : scales.utils.Equiv[QName] = new scales.utils.Equiv[QName]
  implicit def toQName(attribQName: AttributeQName): QName = attribQName.asInstanceOf[QName] // otherwise it blows the stack calling itself
  implicit val toQNameF = (a: Attribute) => { a.name : QName }
}

trait XmlTypesImplicits {

  implicit def toAttrQNameN(nons: NoNamespaceQName): AttributeQName = nons

  implicit def toAttrQNameP(prens: PrefixedQName): AttributeQName = prens

  implicit def toAttr(pair: (String, String))(implicit ver: XmlVersion, fromParser : FromParser) = Attribute(NoNamespaceQName(pair._1)(ver, fromParser), pair._2)

// these was a mistake to directly expose
//  implicit val aqnameEqual = EqualsHelpers.aqnameEqual
//  implicit val toAQNameF = (a: Attribute) => { a.name }
  implicit val qnameEqual = EqualsHelpers.qnameEqual
  implicit val toQNameF = EqualsHelpers.toQNameF
  implicit val qnameEquiv : scales.utils.Equiv[QName] = EqualsHelpers.qnameEquiv

  /**
   * which ever it is, is still a qname
   */
  implicit def toQName(attribQName: AttributeQName): QName = EqualsHelpers.toQName(attribQName)

  /**
   * Converts AQN -> String into an attribute
   */ 
  implicit def aqpairToAttribute(pair : (AttributeQName, String)) = Attribute(pair._1, pair._2)

  import scales.utils._

  /**
   * Provided for those who don't care about the rest
   */
  implicit def docToTree(doc: Doc): XmlTree = doc.rootElem

  /**
   * Default cbf for xml trees
   */ 
  implicit val xmlCBF = implicitly[XmlCBF]

  /**
   * Implicit manifest for sorting positions, big silent cpu eater otherwise, just like the builders.
   */ 
  implicit val xpathSortingClassManifest = implicitly[ClassManifest[(scales.utils.Position[XmlItem,Elem,XCC], XmlPath)]]

  /**
   * Defaults to NotFromParser
   */ 
  implicit val fromParserDefault : FromParser = NotFromParser

  implicit object AttributeOrdering extends scala.math.Ordering[Attribute] {
    def compare(at1 : Attribute, at2 : Attribute) : Int = {
      // don't use toQName
      val aq1 = at1.name : QName
      val aq2 = at2.name : QName

      if ((aq1 eq aq2) || (aq1 =:= aq2)) 
	0
      else {// do each field seperately no string concats.., don't look at prefix, namespaces are likely shared across docs, so check local first
//	....
	val l = aq1.local.compare( aq2.local )
	if (l == 0) {
	  aq1.namespace.uri.compare( aq2.namespace.uri )
	} else l
      }
    }
  }

  /**
   * Only used for lookups
   */
  implicit def toAttrS(local: String)(implicit ver: XmlVersion): Attribute = Attribute(toAttrQNameN(NoNamespaceQName(local)), "")

  /* we don't know if its a CanHavePrefix and we don't care as the prefix we add will be ignored
  implicit val toAttrQ = (nons: QName) => 
    Attribute(PrefixedQName(nons.local, nons.namespace.prefixed("f")(Xml10,IsFromParser))(Xml10,IsFromParser), "")
*/
  implicit def toAttrKS(local: String)(implicit ver: XmlVersion): Key[Attribute] = new AttributeKey(local, Default.noNamespace.uri)

  implicit def toAttrKQ(q : QName) : Key[Attribute] =
    new AttributeKey(q.local, q.namespace.uri)

  implicit val toAttrK = (a : Attribute) => {
    val q = a.name : QName
    new AttributeKey(q.local, q.namespace.uri)
  }

}

// localName to avoid implicits
class AttributeKey(val localName : String, val ns : String) extends Key[Attribute] {
  override def equals( other : Any ) = other match {
    case o : AttributeKey => 
      if (o eq this) true
      else
      (localName == o.localName) && (ns == o.ns)
    case _ => false
  }

  override def hashCode() : Int = {
    var hs = 1
    hs = (hs * 31) + localName.hashCode
    hs = (hs * 31) + ns.hashCode
    hs
  }
}

object ExtraTypesImplicits {
  implicit val toAttrN = (nons: AttributeQName) => Attribute(nons, "")

  implicit def toAttrKAQ(aq : AttributeQName) : Key[Attribute] = {
    val q = aq.asInstanceOf[QName]
    new AttributeKey(q.local, q.namespace.uri)
  }

}

/**
 * Simple constructor for Attributes
 */ 
object Attribs {
  def apply( attribs : Attribute * ) : Attributes = {
    import EqualsHelpers._
    ListSet[Attribute](attribs :_*)
  }
}

sealed trait FromParser

trait XmlTypes {

  import EqualsHelpers._

  private[xml] case object IsFromParser extends FromParser

  private[xml] case object NotFromParserO extends FromParser
  private[xml] val NotFromParser = NotFromParserO 

  import scales.utils.{ Tree, ItemOrTree, subtree, item, top, noPath => fno, Path, TreeCBF, ImmutableArrayProxy }

  type XmlCBF = TreeCBF[XmlItem, Elem, XCC]

  type AttributeQName = EitherLike[PrefixedQName, NoNamespaceQName]

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

  val emptyAttributesHash = emptyAttributes.hashCode
  val emptyNamespacesHash = emptyNamespaces.hashCode

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
    import ExtraTypesImplicits._

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
		       scales.utils.Node( path.node.index, 
			    Tree(parent.section,
				       c :+ n)) )

      // drill down to last child
      Path(tpath, scales.utils.Node(size, n))
    }

  final def addChild(path: XmlPath, child: XmlItem) = {
    import path.node.focus
    val parent = focus.getRight
    Path( path.top, 
	       scales.utils.Node( path.node.index,
		     Tree(parent.section,
        parent.children :+ item[XmlItem, Elem, XCC](child))
			       ))
  }

}

