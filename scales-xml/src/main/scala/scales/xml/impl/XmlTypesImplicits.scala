package scales.xml.impl

import scala.collection.immutable.Map

import scala.collection.generic.CanBuildFrom

//import scales.utils.collection.path.{Path, Node, Position}
import scales.utils.{TreeCBF, subtree, item, top, ItemOrTree, noPath => fno, EitherLike, LeftLike}
import scales.utils.collection.{ListSet, Tree, ImmutableArrayProxy, ImmutableArrayProxyBuilder }
import scales.utils.collection.path.{ Path, Position, Node }

import scales.xml.{NoNamespaceQName, PrefixedQName, AttributeQName, Attribute, XmlTree, Doc, XmlVersion, XmlCBF, XmlPath, Elem, XCC, XmlItem, QName}

import scalaz._
import Scalaz._

import java.nio.charset.Charset

trait XmlTypesImplicits {

  import EqualsHelpers.aqnameEqual

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
  implicit val xpathSortingClassManifest = implicitly[ClassManifest[(Position[XmlItem,Elem,XCC], XmlPath)]]

  /**
   * Defaults to NotFromParser
   */ 
  implicit val fromParserDefault : FromParser = NotFromParser

  /**
   * Only used for lookups
   */
  implicit def toAttrS(local: String)(implicit ver: XmlVersion): Attribute = Attribute(toAttrQNameN(NoNamespaceQName(local)), "")

}
