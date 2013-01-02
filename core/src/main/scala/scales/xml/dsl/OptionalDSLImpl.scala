package scales.xml.dsl

import scales.xml.{
  XmlTree, Elem, 
  XCC, XmlItem, 
  QName, ScalesXml,
  Attribute, emptyChildren,
  ItemOrElem, XPath,
  XmlPath, Text, raw
  }

import scales.utils.collection.path._
import scales.utils.{AsBoolean, subtree, foldPositions, booleanMatcher, booleanAndTMatcher, top, item}

import ScalesXml.xmlCBF

import ScalesXml.fromParserDefault // note cannot be in parser here

/**
 * Entry point to creating OptionalDslBuilders, can be used without the implicit helpers
 */ 
object OptionalDslBuilder {
  
  /**
   * Creates a new tree for the DslBuilder with this @qname for the root elem
   */ 
  def apply( qname : QName ) = new OptionalDslBuilder(DslBuilder.elem2tree(Elem(qname))) 
  /**
   * Creates a new tree for the DslBuilder with @elem as the root
   */
  def apply( elem : Elem ) = new OptionalDslBuilder(DslBuilder.elem2tree(elem))
  /**
   * Uses @tree to directly create the DslBuilder
   */
  def apply( tree : XmlTree ) = new OptionalDslBuilder(tree)
}

/**
 * Represents an optional cascading tree, 
 * 
 * NB Must have a starting element, modelled as tree as we need to keep the data around and trees must always have an elem. 
 */
class OptionalDslBuilder private(val tree: XmlTree) {
  import ScalesXml._

  private implicit def dslToOptional(dsl: DslBuilder): OptionalDslBuilder =
    new OptionalDslBuilder(dsl.toTree)

  /**
   * Add attributes
   */ 
  def ?/@(attribs : => Iterable[Attribute]): OptionalDslBuilder = 
    tree./@(attribs)

  /**
   * Add attributes
   */ 
  def addOptionalAttributes(attribs : => Iterable[Attribute]): OptionalDslBuilder = 
    tree./@(attribs)

  /**
   * Optionally add a single attribute, when None no attribute will be added
   */
  def ?/@(attrib : Option[Attribute]): OptionalDslBuilder = 
    tree./@(attrib)

  /**
   * Optionally add a single attribute, when None no attribute will be added
   */
  def addOptionalAttribute(attrib : Option[Attribute]): OptionalDslBuilder = 
    tree./@(attrib)

  def ?/( itemOrElems : => Iterable[ItemOrElem] ): OptionalDslBuilder =
    tree./(itemOrElems)

  /**
   * Optionally add a child, when None no child will be added
   */
  def ?/( itemOrElem : Option[ItemOrElem] ): OptionalDslBuilder = 
    tree./(itemOrElem)

  /**
   * Optionally add a child, when None no child will be added
   */
  def addOptionalChild( itemOrElem : Option[ItemOrElem] ): OptionalDslBuilder = 
    tree./(itemOrElem)

  /**
   * Optionally add a number of children, when None no child will be added
   */
  def addOptionalChildren( itemOrElem : Option[ItemOrElem] *): OptionalDslBuilder = 
    tree.addOptionals(itemOrElem :_*)

  /**
   * Optionally add a number of children, when None no child will be added
   */
  def addOptionalChildren( itemOrElem : => Iterable[Option[ItemOrElem]] ): OptionalDslBuilder = 
    tree.addOptionals(itemOrElem)

  /**
   * sets the tree to a single Text node child, replacing all others
   */
  def ?~>( value : String ): OptionalDslBuilder = // note ~> is used not -> as it will then get in the way of the tupler
    tree.~>(value)

  /**
   * see ?~>
   */ 
  def setOptionalValue( value : String ): OptionalDslBuilder = ?~>(value)

  /**
   * Optionally sets the tree to a single Text node child, replacing all others, None will not change the current node.
   * 
   * {{{
   *    <(Elem("root"l)) ~> None
   * }}}
   *
   * would leave an empty <root/>
   */
  def ?~>( value : Option[String] ): OptionalDslBuilder = 
    tree.~>(value)

  /**
   * see ~> Option[String]
   */
  def setOptionalValue( value : Option[String] ): OptionalDslBuilder = ?~>(value)

  /**
   * If all of the underly Elem is empty - no children or attributes then it returns None
   */ 
  def toOptionalTree: Option[XmlTree] = 
    if (tree.children.isEmpty && tree.section.attributes.isEmpty)
      None
    else
      Some(tree)

}

