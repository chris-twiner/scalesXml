package scales.xml.impl

//import scales.utils.collection.path.{Path, Node, Position}
import scales.utils.collection.path.Position
import scales.xml.{Attribute, AttributeQName, Doc, Elem, NoNamespaceQName, PrefixedQName, QName, XCC, XmlCBF, XmlItem, XmlPath, XmlTree, XmlVersion}

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
