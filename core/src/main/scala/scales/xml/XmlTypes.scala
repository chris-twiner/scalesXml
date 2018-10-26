package scales.xml

import java.nio.charset.Charset

import scales.utils.LeftLike
import scales.utils.collection.Tree
import scales.xml.impl.{FromParser, IsFromParser, NotFromParser}

import scala.collection.immutable.Map

/**
 * Attributes can only work with either a prefixed qname or an empty ns name
 */
case class Attribute(name: AttributeQName, value: String)

/**
 * Allows easy patterns for attribute values
 */
object Attr {
  def unapply(attr: Attribute) = Some(attr.value)
}

/**
 * Basis for typed pull api
 */
sealed trait XmlEvent

/**
 * The namespaces map refers to sax startPrefixMapping events before this element.  These are used
 * in order to properly write the same XML back out, otherwise we end up with new xmlns:pre declarations
 * for each step.
 *
 * The namespaces map may of course contain a DefaultNamespace.
 *
 * QName -> Attribute as the map functions need the lowest QName to search for not an either
 *
 * @author Chris
 *
 */

import scales.xml.ScalesXml._
import scales.xml.impl.DefaultHashes._

sealed trait Elem extends XmlEvent {
  val name : QName
  def attributes : Attributes
  def namespaces : Map[String, String] = emptyNamespaces

  def copy(name: QName = name, attributes: Attributes = attributes, namespaces: Map[String, String] = namespaces)(implicit fromParser : FromParser) : Elem

  override def equals( other : Any ) = other match {
    case o : Elem => 
      if ((name ==== o.name) // exact match needed?? 
	  && (attributes == o.attributes)
	  && (namespaces == o.namespaces) )
	true
      else
	false
    case _ => false
  }

  override def hashCode() : Int = {
    var hs = 1
    hs = (hs * 31) + name.hashCode
    hs = (hs * 31) + (
      if (emptyAttributes eq attributes)
	emptyAttributesHash // don't do it more than once
      else
	attributes.hashCode
      )
    hs = (hs * 31) + (
      if (namespaces eq emptyNamespaces)
	emptyNamespacesHash // don't do it more than once
      else
	namespaces.hashCode
      )

    hs    
  }

  override def toString(): String = {
    var sb = new java.lang.StringBuilder()
    sb.append("Elem(").
       append(name)
    if (emptyAttributes ne attributes)
      sb.append(", ").
	append(attributes)
    if (emptyNamespaces ne namespaces)
      sb.append(", ").
	append(namespaces)

    sb.append(")").toString()
  }
}

object Elem {

  def apply(name : QName, attributes : Attributes, namespaces :  Iterable[ PrefixedNamespace ] )(implicit fromParser : FromParser) : Elem = 
    apply(name, attributes, namespaces.map( p => p.prefix -> p.ns.uri ).toMap)(fromParser)

  def apply(name : QName, namespaces :  Iterable[ PrefixedNamespace ] )(implicit fromParser : FromParser) : Elem = 
    apply(name, emptyAttributes, namespaces.map( p => p.prefix -> p.ns.uri ).toMap)(fromParser)

  def apply(name : QName , namespace : PrefixedNamespace, namespaces : PrefixedNamespace * )(implicit fromParser : FromParser) : Elem = 
    apply(name, emptyAttributes, (namespace +: namespaces).map( p => p.prefix -> p.ns.uri ).toMap)(fromParser)

  def apply(name : QName, attributes : Attributes, namespace : PrefixedNamespace, namespaces : PrefixedNamespace * )(implicit fromParser : FromParser) : Elem = 
    apply(name, attributes, (namespace +: namespaces).map( p => p.prefix -> p.ns.uri ).toMap)(fromParser)

  private[Elem] final class NoNamespacesElem( namei : QName, attributesi : Attributes ) extends Elem {
    // most elements won't need to redefine, if its present we'll keep it.  Perhaps another option or optimiser should take care of this (i.e. already defined in the document we won't need to keep it around).

    val name = namei
    val attributes = attributesi
    
    def copy(name: QName = name, attributes: Attributes = attributes, namespaces: Map[String, String] = namespaces)(implicit fromParseri : FromParser) : Elem =
      apply(name, attributes, namespaces)(fromParseri)

  }

  private[Elem] final class QNameOnlyElem( namei : QName ) extends Elem {
    val name = namei
    def attributes = emptyAttributes
    
    def copy(name: QName = name, attributes: Attributes = attributes, namespaces: Map[String, String] = namespaces)(implicit fromParseri : FromParser) : Elem =
      apply(name, attributes, namespaces)(fromParseri)
  }

  private[Elem] final class FullElem(namei: QName, attributesi: Attributes, namespacesi: Map[String, String]) extends Elem {

    val name = namei
    val attributes = attributesi
    override val namespaces = namespacesi

    def copy(name: QName = name, attributes: Attributes = attributes, namespaces: Map[String, String] = namespaces)(implicit fromParseri : FromParser) : Elem =
      apply(name, attributes, namespaces)(fromParseri)

  }

  private[Elem] final class NoAttribsElem(namei: QName, namespacesi: Map[String, String]) extends Elem {

    val name = namei
    def attributes = emptyAttributes
    override val namespaces = namespacesi

    def copy(name: QName = name, attributes: Attributes = attributes, namespaces: Map[String, String] = namespaces)(implicit fromParseri : FromParser) : Elem =
      apply(name, attributes, namespaces)(fromParseri)

  }

//  @inline doesn't anything measurable here
  /**
   * Dependent on the inputs creates the smallest footprint implementation of Elem.
   */
  def apply(namei: QName, attributesi: Attributes = emptyAttributes, namespacesi: Map[String, String] = emptyNamespaces)(implicit fromParser : FromParser) : Elem = {
//    checkElemName(namei)(fromParser)
    if (fromParser eq NotFromParser) {
      require(!(namei.prefix.map { p =>
	(p eq PrefixedNamespace.xmlPRE) ||
				  (p eq PrefixedNamespace.xmlnsPRE)
				}.getOrElse(false)), "Prefixes (xmlns, xml) are not allowed for elements")
    }

    if (namespacesi.isEmpty)
      if (!attributesi.isEmpty)
	new NoNamespacesElem( namei, attributesi )
      else 
	new QNameOnlyElem( namei )
    else
      if (!attributesi.isEmpty)
	new FullElem( namei, attributesi, namespacesi )
      else
	new NoAttribsElem( namei, namespacesi )
  }

  def unapply( el : Elem) = Some((el.name, el.attributes, el.namespaces))
}

sealed trait XmlItem extends LeftLike[XmlItem, Tree[XmlItem, Elem, XCC]] with XmlEvent {
  val value: String

  override def hashCode() : Int = 
    value.hashCode

  def doEquals( other : Any, clazz : Class[_] ) = 
    if (clazz.isInstance(other)) {
	val o = other.asInstanceOf[XmlItem]
	if (o eq this) true
	else (o.value == value)
    } else false

}

case class Text(value: String) extends XmlItem

/**
 * Comments aren't escaped and will throw a CommentCannotBeEncoded error if the contents cannot be serialized, xalan just tries to serialize directly which can't work.
 *
 * Note: Will throw if "--" is found in the value
 */
trait Comment extends XmlItem {
  override def equals( other : Any ) = doEquals(other, Comment.commentClass)
}

object Comment {

  val commentClass = Comment("a")(IsFromParser).getClass

  def apply(valuei : String)(implicit fromParser : FromParser) = new Comment {
    if (fromParser eq NotFromParser) 
      require(valuei.indexOf("--") == -1, "Comments cannot contain the sequence --")

    val value = valuei
  }

  def unapply(cmt : Comment) = Some(cmt.value)
}

/**
 * Might be tempting to use, but think twice, XOM removes it for a
good reason, and I'm sorely tempted to, but I've worked with enough
applications that really thought they needed it.
 * CData serialization is dependent on the encoding, if your CData contains
umlauts etc but you choose US-ACSII you will receive a
CDataCannotBeEncoded for your efforts.  Choose not to use CData in the
first place.
 * Note: Will throw if given the CData end sequence
 */
trait CData extends XmlItem {
  override def equals( other : Any ) = doEquals(other, CData.cdataClass)
}

object CData {
  val cdataClass = CData("")(IsFromParser).getClass

  def apply(valuei : String)(implicit fromParser : FromParser) = new CData {
    if (fromParser eq NotFromParser)
      require(valuei.indexOf("]]>") == -1, 
	"CData sections cannot contain the sequence ]]>")
 
    val value = valuei
  }

  def unapply(cmt : CData) = Some(cmt.value)
}

trait PI extends XmlItem {

  override def equals( other : Any ) = 
    if (other.isInstanceOf[PI]) {
      val o = other.asInstanceOf[PI]
      (o.value == value) && (o.target == target)
    } else false

  override def hashCode() : Int = {
    var hs = 1
    hs = (hs * 31) + value.hashCode
    hs = (hs * 31) + target.hashCode
    hs    
  }

  val target : String
}

object PI {

  def apply(targeti : String, valuei : String)(implicit fromParser : FromParser) = new PI {
    if (fromParser eq NotFromParser) {
      require(valuei.indexOf("?>") == -1, "PI Processing Instructions cannot contain the sequence ?> in the value")
      require(targeti.indexOf("?>") == -1, "PI Processing Instructions cannot contain the sequence ?> in the value")
      require(!targeti.toLowerCase.startsWith("xml"), "PI Processing Instructions targets cannot start with ?>")
    }

    val value = valuei
    val target = targeti
  }

  def unapply(pi : PI) = Some((pi.target, pi.value))
}

/**
 * Xml declaration
 */
case class Declaration(version: XmlVersion = defaultVersion, encoding: Charset = scales.utils.defaultCharset, standalone: Boolean = false)

/**
 * DTD is not part of the tree, nor a stream, it can only occur once and as such is neither an XmlEvent nor an XmlItem directly
 */
case class DTD(name: String, publicId: String, systemId: String)

/**
 * Includes all information before the root elem
 */
case class Prolog(
  decl: Declaration = Declaration(),
  misc: Miscs = emptyMiscs,
  dtd: Option[DTD] = None)

/**
 * The information after the root elem
 */
case class EndMisc(misc: Miscs = emptyMiscs)

/**
 * All documents look similar to this, even when stream
 */
trait DocLike {
  def prolog: Prolog
  def end: EndMisc
}

/**
 * Provides a simple empty document for streams etc
 */
case class EmptyDoc(prolog: Prolog = Prolog(), end: EndMisc = EndMisc()) extends DocLike

/**
 * Tree based document
 */
case class Doc(rootElem: XmlTree, prolog: Prolog = Prolog(), end: EndMisc = EndMisc()) extends DocLike {
  override def toString() = "scales.xml.doc"
}

/**
 * Exists purely to satisfy staxs events and indicate to the client code that the xml "stack" should be popped
 */
case class EndElem(name: QName, namespaces: Map[String, String] = Map[String, String]())


/**
 * Simple constructor for Attributes
 */ 
object Attribs {
  def apply( attribs : Attribute * ) : Attributes = {
    //ListSet[Attribute](attribs :_*)
    emptyAttributes ++ attribs
  }
}
