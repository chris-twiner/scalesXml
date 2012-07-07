package scales.xml.jaxen

import org.jaxen._
import pattern.Pattern
import expr._

import java.util.{Iterator => JIterator}

import scales.utils._
import ScalesUtils._
import scales.xml._
import ScalesXml._

import scala.collection.JavaConversions._

//TODO - get a type class in here, looks like doclike usage from the serializers, smells like re-use to me

object Implicits {
  import scalaz._
  import Scalaz._

  import PositionalEquals.{xpathPositionalEqual => xpathEqual}

  /**
   * Equal type class for either A or X
   */ 
  implicit val eitherAOrXEqual = 
    equal[Either[AttributePath, XmlPath]] {
      (a , b) => 
	  if (a.isLeft && b.isLeft) {
	    val al = a.left.get 
	    val bl = b.left.get
	    if (al eq bl) 
	      true
	    else 
	      if (al.attribute == bl.attribute)
		// only now look at the parents
		xpathEqual.equal(al.parent, bl.parent)
	      else
		false
	      
	  } else 
	    if (a.isRight && b.isRight)
	      xpathEqual.equal(a.right.get, b.right.get)
	    else
	      false
    }
}

/**
 * Useful constructors for String XPaths
 */ 
object ScalesXPath {

  /**
   * Identity, no conversion takes place
   */ 
  val defaultNoConversion : QName => QName = identity

  /**
   * Strips a qname of all prefixes and namespaces
   */ 
  val localOnly : QName => QName = q => NoNamespaceQName(q.local)

  def apply(xpath : String, nsMap : Map[String, String] = Map()) =
    new ScalesXPath( xpath, nsMap)

  /**
   * Use PrefixedNamespaces to create the namespace map
   */ 
  def apply(xpath : String, nsMap : Iterable[ PrefixedNamespace ] ) =
    new ScalesXPath(xpath, nsMap.map( p => p.prefix -> p.ns.uri ).toMap)

  /**
   * Use PrefixedNamespaces to create the namespace map, with a variable parameter list 
   */ 
  def apply(xpath : String, pre : PrefixedNamespace, pres : PrefixedNamespace * ) =
    new ScalesXPath(xpath, (pre +: pres).map( p => p.prefix -> p.ns.uri ).toMap, defaultNoConversion)

}

/**
 * Use evaluate every time, select nodes will not work.
 *
 * NB: This class is only available with the scales-jaxen dependencies.
 * 
 * @param nsMap a prefix -> namespace map.  Use the companion object to pass in PrefixedNamespaces
 */ 
class ScalesXPath(val xpath : String, val nsMap : Map[String,String] = Map(), val nameConversion : QName => QName = ScalesXPath.defaultNoConversion) extends ScalesBaseJaxenXPath(xpath, ScalesXPathFactory, new ScalesNavigator(nameConversion)) {
  import Implicits._

  {
    val ns = new SimpleNamespaceContext()
    nsMap.foreach{ (t) => 
      ns.addNamespace(t._1,t._2)
		}
    setNamespaceContext(ns)
  }

  /**
   * Provides a conversion function for QNames, this allows a caller to remove namespaces, for example via ScalesXPath.localOnly
   */
  def withNameConversion( conversion : QName => QName ) =
    new ScalesXPath( xpath, nsMap, conversion)

  /**
   * Jaxen can't sort non identity/reference equality based object models.  So we do the work for it and cut out the existing sorting.
   *
   * Additionally its nice to get a somewhat better result
   */
  def evaluate( path : XmlPath ) : Iterable[Either[AttributePath, XmlPath]] = {
    val res = selectNodes(path).asInstanceOf[java.util.List[AnyRef]]
    if (res.size < 2) 
      if (res.size == 0) 
	Nil
      else {
	val it = res.get(0)
	if (it eq null) 
	  Nil
	else
	  one(it match {
	    case x @ DocsUp(a : AttributePath, p) => Left(a)
	    case x @ DocsUp(xp : XmlPath, p) => Right(xp)
	    case DocumentRoot(r) => Right(r)
	  })
      }
    else
      DuplicateFilter(sortT[XmlItem, Elem, XCC, Either[AttributePath,XmlPath]](res.map{
	case x @ DocsUp(a : AttributePath, p) => (Left(a), a.parent)
	case x @ DocsUp(xp : XmlPath, p) => (Right(xp), xp)
	case DocumentRoot(r) => (Right(r), r)
	}).map{x => x._1})(eitherAOrXEqual)
  }

  /**
   * Evaluates path returning only matching XmlPaths
   */ 
  def xmlPaths( path : XmlPath ) : Iterable[XmlPath] =
    evaluate(path).collect{ case Right(x) => x}

  /**
   * Evaluates path returning only matching AttributePaths
   */ 
  def attributePaths( path : XmlPath ) : Iterable[AttributePath] =
    evaluate(path).collect{ case Left(x) => x}

  /**
   * Single primitive version with hidden cast :<
   */ 
  def get[T]( path : XmlPath ) : T = {
    val res = evaluate(path : Object)
    res.asInstanceOf[T]
  }
}

/**
 * Interesting exercise in futility, its better not to sort as so many other places make reference / identity assumptions.
 */ 
object ScalesComparator extends java.util.Comparator[AnyRef] {
  
  val nav = new ScalesNavigator(ScalesXPath.defaultNoConversion) 
  import nav._

  def compare( o1 : AnyRef, o2 : AnyRef) = {
    if (isNonChild(o1) && isNonChild(o2)) {
      val p1 = getParentNode(o1)
      val p2 = getParentNode(o2)
			
      if (p1 == p2) {
	if (isNamespace(o1) && isAttribute(o2))
	  -1
	else 
	  if (isNamespace(o2) && isAttribute(o1))
	    1
	  else compare(p1,p2)
      } else compare(p1,p2)      
    } else {
      def getPath(o : AnyRef) = 
	if (isAttribute(o))
	  (o : AttributePath).parent 
	else
	  if (isDocument(o))
	    o.asInstanceOf[DocumentRoot].xmlPath
	  else
	    o : XmlPath

      val path1 = getPath(o1)
      val path2 = getPath(o2)

      // reverse it
      0 - comparePathPositions(path1.position, path2.position)
    }
  }

  def isNonChild( o : AnyRef) = 
    isAttribute(o) || isNamespace(o)

}

/**
 * Due to reference equality comparisoms and a deeply woven comparator we need to override a few things...
 */ 
object ScalesXPathFactory extends DefaultXPathFactory {

  override def createUnionExpr( lhs : Expr, rhs : Expr ) =
    new ScalesUnionExpr( lhs, rhs )

  override def createRelativeLocationPath() = 
    new ScalesDefaultLocationPath()//DefaultRelativeLocationPath()
   
  override def createAbsoluteLocationPath() = 
    new ScalesDefaultAbsoluteLocationPath()

}

/**
 * Maps Scales Xml into Jaxen
 * @param nameConversion allows conversion of a qname in the document before evaluation
 */
class ScalesNavigator(val nameConversion : QName => QName) extends DefaultNavigator {

  /**
   * I dislike typing this over and over again
   */
  implicit def fromDocsUpX( ctx : AnyRef ) : XmlPath = 
    ctx.asInstanceOf[DocsUp[XmlPath]].what
  implicit def fromDocsUpA( ctx : AnyRef ) : AttributePath = 
    ctx.asInstanceOf[DocsUp[AttributePath]].what

  def use[T,W]( ctx : AnyRef, f : DocsUp[W] => T ) = 
    f( ctx.asInstanceOf[DocsUp[W]] )

  def wrap[W]( ctx : AnyRef, f : DocsUp[W] => W ) =
    use( ctx, (d : DocsUp[W]) => d.copy( what = f(d)))

  override def getChildAxisIterator( ctx : AnyRef ) = 
    ctx match {
      case dr @ DocumentRoot(r) => one(DocsUp(r,dr)).iterator
      case DocsUp(x : XmlPath,d) => x.map(DocsUp(_,d)).iterator
      case _ => error("couldn't get childaxis")
    }

  override def getParentAxisIterator( ctx : AnyRef ) =
    one(getParentNode(ctx)).iterator

  override def getAttributeAxisIterator( ctx : AnyRef ) =
    ctx match {
      case DocsUp(x : XmlPath, r) if (!x.isItem) => 
	use(ctx, (d : DocsUp[XmlPath]) => 
	  d.what.tree.section.
	    attributes.map(a => DocsUp(
	      AttributePath(a, ctx), 
	      d.docroot)).iterator)
      case _ => Nil.iterator
    }
  
  override def getParentNode( ctx : AnyRef ) =
    ctx match {
      case DocsUp(AttributePath(a, x), d) => DocsUp(x,d)
      case DocsUp(x : XmlPath, d) => 
	if (x eq d.xmlPath)
	  d
	else
	  DocsUp(x.zipUp,d)
      case DocumentRoot(x) => null
    }

  def parseXPath( xpath : String ) = new ScalesXPath(xpath)
  
  def getNamespacePrefix( ctx : AnyRef ) = error("no namespace nodes yet")
  def getNamespaceStringValue( ctx : AnyRef ) = error("no namespace nodes yet")
  def isNamespace( ctx : AnyRef ) = false//error("no namespace nodes yet")

  def getTextStringValue( ctx : AnyRef ) = TextFunctions.value(ctx)

  // attributes
  def getAttributeStringValue( ctx : AnyRef ) = ctx match {
    case DocsUp(AttributePath(a, x),d) => a.value
    case _ => error("not an attribute")
  }
  def isAttribute( ctx : AnyRef ) = 
    if (ctx.isInstanceOf[DocsUp[_]]) {
      val docsUpWhat = ctx.asInstanceOf[DocsUp[_]].what
      if (docsUpWhat.isInstanceOf[AttributePath])
	true
      else false
    } else false
  
  def getAttributeQName( ctx : AnyRef ) =  ctx match {
    case DocsUp(AttributePath(a, x),d) => 
      if (nameConversion eq ScalesXPath.defaultNoConversion)
	a.name.qName
      else
	nameConversion(a.name).qName
    case _ => error("not an attribute")
  }
  def getAttributeName( ctx : AnyRef ) = ctx match {
    case DocsUp(AttributePath(a, x),d) => 
      if (nameConversion eq ScalesXPath.defaultNoConversion)
	a.name.local
      else
	nameConversion(a.name).local
    case _ => error("not an attribute")
  }
  def getAttributeNamespaceUri( ctx : AnyRef ) = ctx match {
    case DocsUp(AttributePath(a, x),d) => 
      if (nameConversion eq ScalesXPath.defaultNoConversion)
	a.name.namespace.uri
      else
	nameConversion(a.name).namespace.uri
    case _ => error("not an attribute")
  }

  def getCommentStringValue( ctx : AnyRef ) = TextFunctions.value(ctx)

  def pOr[T]( ctx: AnyRef, f : XmlPath => T, e : => T) = 
    if (ctx.isInstanceOf[DocsUp[_]]) {
      val docsUpWhat = ctx.asInstanceOf[DocsUp[_]].what
      if (docsUpWhat.isInstanceOf[XmlPath])
	f(docsUpWhat.asInstanceOf[XmlPath])
      else e
    } else e//{ println("Or for ctx "+ctx);e}

  def pOrFalse(ctx : AnyRef, f : XmlPath => Boolean) = 
    pOr(ctx, f, false)

  def iOrFalse(ctx : AnyRef, f : XmlPath => Boolean) =
    pOr(ctx, (x) => if (x.isItem) f(x) else false, false)

  def isProcessingInstruction( ctx : AnyRef ) = iOrFalse(ctx, _.item.isInstanceOf[PI])
  def isText( ctx : AnyRef ) = iOrFalse(ctx, x => 
    x.item.isInstanceOf[Text] || 
    x.item.isInstanceOf[CData] )
  def isComment( ctx : AnyRef ) = iOrFalse(ctx, _.item.isInstanceOf[Comment])
  def isDocument( ctx : AnyRef ) = ctx.isInstanceOf[DocumentRoot]
  
  // elements

  def isElement( ctx : AnyRef ) = pOrFalse(ctx,!_.isItem)

  def getElementStringValue( ctx : AnyRef ) = 
    if (isElement(ctx))
      Elements.Functions.text(ctx)
    else null

// ScalesXPath.defaultNoConversion
  def getElementQName( ctx : AnyRef ) = 
    if (nameConversion eq ScalesXPath.defaultNoConversion)
      ctx.tree.section.name.qName
    else
      nameConversion(ctx.tree.section.name).qName
    
  def getElementName( ctx : AnyRef ) = 
    if (nameConversion eq ScalesXPath.defaultNoConversion)
      ctx.tree.section.name.local
    else
      nameConversion(ctx.tree.section.name).local

  def getElementNamespaceUri( ctx : AnyRef ) = 
    if (nameConversion eq ScalesXPath.defaultNoConversion)
      ctx.tree.section.name.namespace.uri
    else
      nameConversion(ctx.tree.section.name).namespace.uri
    
  override def getDocument( uri : String ) = error("don't do doc lookups at all man")

  override def getDocumentNode( ctx : AnyRef ) =
    DocumentRoot(ctx.asInstanceOf[XmlPath])

/*  override def getNodeType( node : AnyRef ) = {
    val p : XmlPath = node
    if (p.isItem) {
      val i = p.item
      i match {
	case _ : Text => Pattern.TEXT_NODE
	case _ : Comment => Pattern.COMMENT_NODE
	case _ : PI => Pattern.PROCESSING_INSTRUCTION_NODE
	case _ : CData => Pattern.CDATA_SECTION_NODE
      }
    } else Pattern.ELEMENT_NODE

    if ( isElement(node) ) 
      {
        return Pattern.ELEMENT_NODE;
      }
    else if ( isAttribute(node) ) 
      {
        return Pattern.ATTRIBUTE_NODE;
      }
    else if ( isText(node) ) 
      {
        return Pattern.TEXT_NODE;
      }
    else if ( isComment(node) ) 
      {
        return Pattern.COMMENT_NODE;
      }
    else if ( isDocument(node) ) 
      {
        return Pattern.DOCUMENT_NODE;
      }
    else if ( isProcessingInstruction(node) ) 
      {
        return Pattern.PROCESSING_INSTRUCTION_NODE;
      }
    else if ( isNamespace(node) ) 
      {
        return Pattern.NAMESPACE_NODE;
      }
    else {
      return Pattern.UNKNOWN_NODE;
    }
  }*/
}
