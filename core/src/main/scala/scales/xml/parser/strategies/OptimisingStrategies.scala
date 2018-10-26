package scales.xml.parser.strategies

import scales.xml._
import scales.xml.impl.{FromParser, TreeProxies}

trait OptimisingStrategiesImplicits {

}

/**
 * Simple marker for per parse optimisation oppurtunties
 */
trait OptimisationToken {
  implicit val ver : XmlVersion
  implicit val fromParser : FromParser

  /**
   * cached attribute array
   */
  private[strategies] var attrs: Array[Attribute] = Array.ofDim(20)  

  /**
   * resizes the cached attribute array by 50% if needed
   */ 
  private[strategies] final def resizeAttrs(size: Int) {
    if (size > attrs.length) {
      attrs = Array.ofDim((size * 1.5).toInt)
    }
  }
}

class BaseToken(implicit val ver : XmlVersion, val fromParser : FromParser) extends OptimisationToken

/**
 * Memory usage in DOMs is often dominated by repeated elements.  Xerces and co use string tables to optimise memory usage, with DTM a key example.
 *
 * Performing lookups is expensive so the strategies can have selective levels of lookup.  Developers may therefore choose appropriate levels that best fit their trade-off between space and time.
 *
 * There will, of course, be temporary garbage created for such a scheme but it should pay off for larger messages.
 *
 */ 
trait MemoryOptimisationStrategy[Token <: OptimisationToken] {

  def createToken(implicit ver : XmlVersion, fromParser : FromParser) : Token

  /**
   * During parsing provides a backing array for attribute storage.  The expected new size 
   * must be passed in, failure to give a size large enough will lead to OOB exceptions.
   *
   * Implementations are advised to use the array for parsing performance reasons.
   *
   * The default implementataion expands the size by 50% of the backed array.  The array ownership should remain with the strategy instance
   */
  def attributeArray(newSize: Int, token: Token): Array[Attribute] = {
    token.resizeAttrs(newSize)
    token.attrs
  }

  /**
   * It is expected that certain attributes have fixed values, ie. booleans or based on schema enums etc, this function allows such optimisations.
   *
   * The qname will have been obtained via a call to either noNamespaceQName, unprefixedQName or prefixedQName, so any optimisations provided by them can be leveraged.
   *
   */ 
  def attribute( qname : AttributeQName, value : String, token : Token) : Attribute = Attribute(qname,value)

  def noNamespaceQName( local : String, token : Token) : NoNamespaceQName = {
    import token._
    NoNamespaceQName(local)
  }

  def unprefixedQName( local : String, uri : String, token : Token) : UnprefixedQName =  {
    import token._
    UnprefixedQName(local, Namespace(uri))
  }

  def prefixedQName( local : String, uri : String, prefix : String, token : Token) : PrefixedQName =  {
    import token._
    PrefixedQName(local, Namespace(uri).prefixed(prefix))
  }

  /**
   * For files without large (and varied) attribute counts per repeated element it may represent significant space savings to optimise against the entire Elem itself.
   *
   * The Elements QName is garaunteed to be the last QName evaluated via the QName functions (i.e. Attributes are done first then the Elems QName)  
   */ 
  def elem( name : QName, attributes : Attributes, namespaces : Map[String, String], token : Token) : Elem =  {
    import token._
    Elem(name, attributes, namespaces)
  }
}

trait BaseTokenF {

  def createToken(implicit ver : XmlVersion, fromParser : FromParser) : BaseToken = new BaseToken()

}

/**
 * Performs no optimisation at all
 */ 
object NoOptimisation extends PathOptimisationStrategy[BaseToken] with BaseTokenF

/**
 * Certain paths may be repeated (based on QNames of parents etc) and known to be by the developer, its also possible that the developer simply is not interested in this path.
 *
 * Developers may also customise the creation of paths, for example adding starting children, replacing the XmlChildren implementation or replacing entire subtrees.
 */
trait PathOptimisationStrategy[Token <: OptimisationToken] extends MemoryOptimisationStrategy[Token] {
  
  /**
   * By default calls TreeProxies.elementEnd.  Replacing entire subtrees can be performed here.
   */ 
  def elementEnd( xml : TreeProxies, token : Token) {
    xml.elementEnd
  }

  /**
   * Start a new "tree", pushing the elem to TreeProxies
   */ 
  def beginSubTree( stack : TreeProxies, elem : Elem, token : Token) {
    stack.beginSub(elem, XmlBuilder())
  }
}

import scales.xml.impl.FullEqualQNameKey

class QNameToken(implicit val ver : XmlVersion, val fromParser : FromParser) extends OptimisationToken {
  val qkey = new FullEqualQNameKey()

  // the following reduce init costs was 6% of garbage and 3% of runtime on avg

  val noNsQ = (k : FullEqualQNameKey) => NoNamespaceQName(k.local)
  val unQ = (k : FullEqualQNameKey) => UnprefixedQName(k.local, Namespace(k.namespace))
  val pQ = (k : FullEqualQNameKey) => PrefixedQName(k.local, Namespace(k.namespace).prefixed(k.prefix))
  
}

trait QNameTokenF {
  def createToken(implicit ver : XmlVersion, fromParser : FromParser) = new QNameToken()
}

/**
 * Threadsafe global cache, assumption is most applications would benefit from this 
 */
trait QNameOptimisationT[Token <: QNameToken] extends MemoryOptimisationStrategy[Token] {

  import java.util.concurrent.ConcurrentHashMap

  val qNameCache = new ConcurrentHashMap[ FullEqualQNameKey, QName ]
  
  /**
   * The creation costs can also be reduced
   */ 
  override def noNamespaceQName( local : String, token : Token) : NoNamespaceQName = {
    val qt = token
    import qt._
    value( qkey.setNoNamespaceQName(local) )( noNsQ )
  }

  override def unprefixedQName( local : String, uri : String, token : Token) : UnprefixedQName = {
    val qt = token
    import qt._
    value( qkey.setUnprefixedQName(local, uri) )( unQ )
  }

  override def prefixedQName( local : String, uri : String, prefix : String, token : Token) : PrefixedQName = {
    val qt = token
    import qt._
    value( qkey.setPrefixedQName( local, uri, prefix) )( pQ )
  }

  def value[T <: QName](key : FullEqualQNameKey)( newT : FullEqualQNameKey => T ) : T = {
    var value = qNameCache.get(key)
    if (value == null) {
      value = newT(key)
      val res = qNameCache.putIfAbsent(key.copy, value)// copy because now it needs to be frozen
      value = if (res == null) value else res
    }
    value.asInstanceOf[T]
  }

}

/**
 * The default as it will equal or better Scala Xml memory consumption at a performance gain.
 *
 * For the lowest memory consumption possible, for example where memory is more important than raw performance, see QNameElemTreeOptimisation
 *
 * @see scales.xml.strategies.QNameElemTreeOptimisation
 */
object QNameMemoryOptimisation extends PathOptimisationStrategy[QNameToken] with QNameOptimisationT[QNameToken] with QNameTokenF
