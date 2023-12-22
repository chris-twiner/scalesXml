package scales.xml.parser.strategies

import scales.xml._
import scales.utils.collection.{SeqLikeThing, Tree}
import scales.utils.{ItemOrTree, valueOf}
import scales.xml.impl.{FromParser, NotFromParser}
import impl.ElemKey

class ElemToken(implicit ver : XmlVersion, fromParser : FromParser) extends QNameToken {
  val ekey = new ElemKey()
  
  val ef = (key : ElemKey) => Elem(key.name, key.attributes, key.namespaces)
}

trait ElemTokenF {
  def createToken(implicit ver : XmlVersion, fromParser : FromParser) = new ElemToken()
}


trait ElemQNameOptimisationT[Token <: ElemToken] extends QNameOptimisationT[Token] {
  import java.util.concurrent.ConcurrentHashMap

  val ecache = new ConcurrentHashMap[ ElemKey, Elem ]

  val cacheAll = false

  def elemValue(key : ElemKey)( newT : ElemKey => Elem ) : Elem = {
    var value = ecache.get(key)
    if (value == null) {
      value = newT(key)
      val res = ecache.putIfAbsent(key.copy, value)// copy because now it needs to be frozen
      value = if (res == null) value else res
    }
    value
  }

  override def elem( name : QName, attributes : Attributes, namespaces : Map[String, String], token : Token) : Elem = { 
    val et = token
    import et._

    if (cacheAll ||
	((attributes eq emptyAttributes) &&
	(namespaces eq emptyNamespaces))
	)
        elemValue(ekey.set(name, attributes, namespaces, qkey.lastHash) )( ef )
    else Elem(name, attributes, namespaces)(token.fromParser)
  }
  
}

trait FullMemoryOptimisationT[Token <: ElemToken] extends ElemQNameOptimisationT[Token] {
  override val cacheAll = true
}


/**
 * Simple hashmap on Elem -> Elem, makes no optimisations across qnames (use QNameOptimisationT for that)
 */ 
trait ElemOptimisationT[Token <: OptimisationToken] extends MemoryOptimisationStrategy[Token] {

  import java.util.concurrent.ConcurrentHashMap

  val cache = new ConcurrentHashMap[ Elem, Elem ]

  override def elem( name : QName, attributes : Attributes, namespaces : Map[String, String], token : Token) : Elem = {
    import token._
    valueOf(Elem(name, attributes, namespaces), cache) (Elem(name, attributes, namespaces))
  }  
}

object ElemMemoryOptimisation extends PathOptimisationStrategy[ElemToken] with ElemOptimisationT[ElemToken] with ElemTokenF

/**
 * Optimises on QNames and Elems, reducing duplicates at the cost of CHM lookups.
 */ 
object HighMemoryOptimisation extends PathOptimisationStrategy[ElemToken] with ElemOptimisationT[ElemToken] with QNameOptimisationT[ElemToken] with ElemTokenF

/**
 * As this adds extra processing time, but better fits XPath rules (i.e. all text children below are joined) its available to be mixed in, but isn't added by default.
 *
 * This also removes any extra parsing time from string joining.
 */ 
trait TextNodeJoiner[Token <: OptimisationToken] extends TreeOptimisation[Token] {

  def newTree( elem : Elem, children : XmlChildren, token : Token )(implicit seqLikeThing: SeqLikeThing[XCC[_], ItemOrTree[XmlItem, Elem, XCC], XCC]) : XmlTree =
    Tree(elem, joinTextNodes(children))

}

