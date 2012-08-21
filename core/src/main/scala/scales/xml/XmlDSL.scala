package scales.xml

import scales.utils._

import ScalesXml.xmlCBF

import ScalesXml.fromParserDefault // note cannot be in parser here

trait DslImplicits {
  import ScalesXml._
  implicit def fromElemToBuilder(elem : Elem) = <(elem)
  implicit def fromQNamePairToAttribute(pair : (PrefixedQName, String)) = Attribute(pair._1, pair._2)

  implicit def fromDslBuilderToTree(dslB : DslBuilder) = dslB.toTree
  
  /**
   * Only works for elems, allows simpler definitions
   */
  implicit def fromQNameToTree( qname : QName) = DslBuilder.q2tree(qname)

  /**
   * Serialisation and other dsl friends benefit from this
   */ 
  implicit def fromElemToTree( elem : Elem ) : XmlTree = DslBuilder.elem2tree(elem)

  /**
   * Allows direct use of text where expected
   */ 
  implicit def fromStringToText( value : String ): Text = Text(value)

  /**
   * Only works for elems, allows simpler definitions.  Not sure of its usefulness outside of tests though
  implicit def fromQNameToItemOrTree( qname : QName) : ItemOrElem = DslBuilder.q2tree(qname)//Right()
   */

  /**
   * Only works for elems, better looking than <
   */
  implicit def fromQNameToBuilder( qname : QName) = <(qname)

  class QNameMatcher(qname : QName) {
    import ScalesXml._
    def unapply( elem : Elem ) : Option[Elem] = if (elem.name =:= qname) Some(elem) else None
    def unapply( attrib : Attribute ) : Option[Attribute] = if (toQName(attrib.name) =:= qname) Some(attrib) else None
  }

  class QNameMPimper( qname : QName ) {
    def m = new QNameMatcher(qname)
    def matcher = m
  }

  /**
   * matches elements and attributes based on qname only
   */
  implicit def fromQNameToQNameMatcher( qname : QName) = new QNameMPimper(qname)

  implicit def fromTreeToDsl( tree: XmlTree ) = DslBuilder(tree)

  class NamespaceMatcher(ns : Namespace) {
    import ScalesXml._
    def unapply( elem : Elem ) : Option[Elem] = 
      if (elem.name.namespace == ns) Some(elem) else None
    def unapply( attrib : Attribute ) : Option[Attribute] = 
      if (toQName(attrib.name).namespace == ns) Some(attrib) else None
  }

  class NSMPimper( ns : Namespace ) {
    def m = new NamespaceMatcher(ns)
    def matcher = m
  }

  implicit def fromNSToNSMPimper( ns : Namespace ) = new NSMPimper(ns)
}

trait XPathMatcher {

  /**
   * Wrap xpaths into matches, gives back the resulting path if its non empty
   */ 
  def pathMatcher[T : AsBoolean]( pathEval : (XmlTree) => T ) = booleanMatcher[XmlTree, T](pathEval)

  
  /**
   * Wrap xpaths into matches, gives back the resulting path if its non empty
   * and the tree that matched it (doesn't force the user to create a val name beforehand)
   */ 
  def pathAndTreeMatcher[T : AsBoolean]( pathEval : (XmlTree) => T ) = booleanAndTMatcher[XmlTree, T](pathEval)

}

object < {
  def apply(qname : QName) = DslBuilder(Elem(qname))
  def apply(elem : Elem) = DslBuilder(elem)
}

protected[xml] object DslBuilder {
  def elem2tree( elem : Elem ) : XmlTree = subtree[XmlItem, Elem, XCC](elem,emptyChildren).right.get
  def q2tree( qname : QName ) = elem2tree(Elem(qname))
  def apply( elem : Elem ) = new DslBuilder(elem2tree(elem))
  def apply( tree : XmlTree ) = new DslBuilder(tree)
}

/**
 * Simple runtime Wrapper around folds
 */ 
case class FoldErrorException(error : FoldError) extends RuntimeException

/*
 * Must have a starting element, modelled as tree as we need to keep the data around and trees must always have an elem
 */
class DslBuilder private( tree : XmlTree ) {
  import ScalesXml._
  /**
   * Add attributes
   */ 
  def /@(attribs : Attribute*) = new DslBuilder( tree.copy(section = tree.section.copy (attributes = tree.section.attributes ++ attribs) ))

  /**
   * Add attributes
   */ 
  def /@(attribs : => Iterable[Attribute]) : DslBuilder = new DslBuilder( tree.copy(section = tree.section.copy (attributes = tree.section.attributes ++ attribs) ))

  /**
   * Optionally add a single attribute, when None no attribute will be added
   */
  def /@(attrib : Option[Attribute]) = 
    attrib.map{ a => 
      new DslBuilder( tree.copy(section = tree.section.copy (attributes = tree.section.attributes + a) ))
	     }.getOrElse{this}

  /**
   * Remove attributes
   */
  def -/@(attribs : QName*) = new DslBuilder( tree.copy(section = tree.section.copy (attributes = tree.section.attributes -- attribs)))
  
  def add( itemOrElems : ItemOrElem * ) = /( itemOrElems :_* )

  def /( itemOrElems : => Iterable[ItemOrElem] ) : DslBuilder =
    new DslBuilder( tree.copy( children = (tree.children ++ itemOrElems  )))

  /**
   * Optionally add a child, when None no child we be added
   */
  def /( itemOrElem : Option[ItemOrElem] ) = 
    itemOrElem.map{ i => 
      new DslBuilder( tree.copy( children = (tree.children :+ i  )))
		 }.getOrElse{this}

  /**
   * Fold over the current tree, allows folding deep within a builder.  Either the fold works or `this` is returned with the FoldError.
   */ 
  def fold[T <: Iterable[XmlPath]]( xpath : XmlPath => XPath[T])( folder: (XmlPath) => FoldOperation[XmlItem, Elem, XCC] ) : Either[DslBuilder, (DslBuilder, FoldError)] = 
    (foldPositions( raw( xpath( top(tree) ) ) )(folder)).
      fold( x => Left(new DslBuilder( x.tree )), y => Right((this,y)) )

  /**
   * fold_! calls fold but throws the error when its returning a root
   */
  def fold_![T <: Iterable[XmlPath]]( xpath : XmlPath => XPath[T])( folder: (XmlPath) => FoldOperation[XmlItem, Elem, XCC] ) : DslBuilder =
    (fold(xpath)(folder)).fold(identity, x => throw new FoldErrorException(x._2))

  /**
   * Does not throw when NoPaths is returned, simply returning this
   */ 
  def fold_?[T <: Iterable[XmlPath]]( xpath : XmlPath => XPath[T])( folder: (XmlPath) => FoldOperation[XmlItem, Elem, XCC] ) : DslBuilder =
    (fold(xpath)(folder)).fold(identity, x => 
      if (x._2 eq NoPaths) 
	this
      else throw new FoldErrorException(x._2))

  /**
   * any qname will do given its elems
   */
  def -/( qname : QName ) = new DslBuilder(tree.copy(children = tree.children.filterNot{
    either =>
      either.fold( item => false, tree => tree.section.name =:= qname)
  }))

  /**
   * Add a number of xmlITems, text, cdata, comments etc, the two params is to get around /(Seq) erasures.
   */
  def /( itemOrElems : ItemOrElem * ) = 
    new DslBuilder( tree.copy( children = (tree.children ++ itemOrElems )))

  /**
   * sets the tree to a single Text node child, replacing all others
   */
  def ~>( value : String ) = // note ~> is used not -> as it will then get in the way of the tupler
    new DslBuilder( tree.copy(children = emptyChildren :+ item[XmlItem, Elem, XCC](Text(value))))

  /**
   * see ~>
   */ 
  def setValue( value : String ) = ~>(value)

  /**
   * Optionally sets the tree to a single Text node child, replacing all others, None will not change the current node.
   * 
   * {{{
   *    <(Elem("root"l)) ~> None
   * }}}
   *
   * would leave an empty <root/>
   */
  def ~>( value : Option[String] ) = 
    value.map{ v =>
      new DslBuilder( tree.copy(children = emptyChildren :+ item[XmlItem, Elem, XCC](Text(v))))
	    }.getOrElse{this}

  /**
   * see ~> Option[String]
   */
  def setValue( value : Option[String] ) = ~>(value)
  
  /**
   * Cleans out any child text nodes (comments, cdata etc) and just leaves child elements
   */
  def elementsOnly = new DslBuilder(tree.copy(children = tree.children.filter( _.fold( _ => false, _ => true ) ) ))

  def toTree = tree

}
