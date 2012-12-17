package scales.xml.impl

import scales.xml.{ItemOrElem, XmlChildren, XmlItem, Text, XmlTree, XmlVersion, ScalesXml, defaultPathOptimisation, Doc, loadXmlReader, convertFromScalaXml, emptyChildren}

import ScalesXml.xmlCBF
import scales.utils.resources.Loaner
  
import scales.xml.parser.strategies.{PathOptimisationStrategy, OptimisationToken}

import scales.xml.parser.sax.SaxSupport

/**
 * A collection of whitespace related functions
 */
trait Whitespace {
  import scales.utils.collection.Tree

//  import scala.collection.generic.CanBuildFrom

  /**
   * XPath normalize-space function, replaces all consecutive whitespace with " " and trims.
   */
  def normalizeSpaceS(str: String) = str.replaceAll("\\s{2,}", " ").trim

  /**
   * Joins adjacent text nodes for immediate children
   */
  val mergeAdjacentText
    : ((Option[XmlItem], XmlChildren), ItemOrElem) => (Option[XmlItem], XmlChildren)
    = (pair: (Option[XmlItem], XmlChildren), item: ItemOrElem) =>
    if (item.isLeft && item.left.get.isInstanceOf[Text])
      // its an item
      // do we have a current text node?
      if (pair._1.isDefined) {
        val text = Text(pair._1.get.value + item.left.get.value)

        // replace the last
        (Some(text), (pair._2.dropRight(1) :+ (text)) : XmlChildren)//Left(
      } else (Some(item.left.get), (pair._2 :+ (item)) : XmlChildren)
    else
      (None, (pair._2 :+ (item)) : XmlChildren)
  
  /**
   * Joins adjacent text nodes for the immediate children only (make a tree more XPath friendly) 
   */
  def joinTextNodes(tree: XmlTree) : XmlTree =
    Tree(tree.section, joinTextNodes(tree.children))

  def joinTextNodes(children : XmlChildren) : XmlChildren =
    children.foldLeft((None: Option[XmlItem], emptyChildren ))(mergeAdjacentText)._2


}

import org.xml.sax.XMLReader

trait XmlUtils {
  
  /**
   * Conversion from Scala XML into Scales XML
   */ 
  def convertFromScalaXml[Token <: OptimisationToken]( elem : scala.xml.Elem, parsers : Loaner[XMLReader] with SaxSupport = DefaultXMLReaderFactoryPool, optimisationStrategy : PathOptimisationStrategy[Token] = defaultPathOptimisation, encoding : String = "UTF-8" )(implicit xmlVer : XmlVersion)  : Doc = {
    // simple stream conversion.., for "large docs" a conversion in place might be better, but for now its isolated....
    var out = new java.io.StringWriter()
    val p = parsers						 
    scala.xml.XML.write(out, elem, encoding, true, null)
    import ScalesXml.readerToSource
    loadXmlReader[Token](new java.io.StringReader(out.toString), parsers = p, strategy = optimisationStrategy)
  }
}

trait XmlUtilsImplicits {
  class ToScales( elem : scala.xml.Elem)(implicit xmlVer : XmlVersion) {
    def asScales[Token <: OptimisationToken](parsers : Loaner[XMLReader] with SaxSupport = DefaultXMLReaderFactoryPool, optimisationStrategy : PathOptimisationStrategy[Token] = defaultPathOptimisation, encoding : String = "UTF-8" ) = convertFromScalaXml(elem, parsers, optimisationStrategy, encoding)
  }

  implicit def toScalesXml( elem : scala.xml.Elem)(implicit xmlVer : XmlVersion) = new ToScales(elem)
}
