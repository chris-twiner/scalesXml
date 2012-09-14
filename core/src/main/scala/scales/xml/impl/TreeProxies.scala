package scales.xml.impl

import scales.xml._
import scales.utils._


// XmlBuilder, XmlChildren
final class TreeProxy( private[this] var _elem : Elem, private[this] val _builder : XmlBuilder){
  @inline def elem = _elem

  @inline def setElem( elem : Elem ) {
    _elem = elem
  }

  @inline def builder = _builder
}

import scala.collection.mutable.ArrayBuffer

/**
 * Mutable list that keeps the item creation to a minimum, no extra garbage here until the parse is done...
 *
 * NOTE this is effectively an internal structure, but is provided for user land performance tweaks
 */ 
class TreeProxies( ){
  import ScalesXml.xmlCBF

  // special case root tree
  var rootTree : XmlTree = _
  
  private[this] var _depth : Int = -1

  private[this] var _proxies : Array[TreeProxy] = Array.ofDim[TreeProxy](50)

  /**
   * Keeps the same _proxies array but resets the rest.  The old proxies is
   * no longer usable.
   * WARN - as per class docs this is effectively an internal structure caveat empor
   */ 
  def reuse : TreeProxies = {
    // size is not redone so we keep builders around
    _depth = -1
    rootTree = null.asInstanceOf[XmlTree]
    _current = null.asInstanceOf[TreeProxy]
    this
  }

  // current max size in the proxies (_proxies.length could be far larger)
  private[this] var _size = 0

  private[this] var _current : TreeProxy = _

/*
 * interface for TreeOptimisations below, don't penalise normal parsing
 */ 
  def current = _current
  def current_=( tp : TreeProxy ) {
    _current = tp 
  }
  def depth = _depth
  def depth_= ( newDepth : Int ) { _depth = newDepth }
  def proxy( depth : Int ) = _proxies( depth )

  def addChild( i : XmlItem ) {
    _current.builder.+=(i)
  }

  def elementEnd() {
    val l = _current

    val newTree = Tree(l.elem, l.builder.result)
    
    if (_depth > 0) {
      _depth -= 1
      _current = _proxies( _depth )
      _current.builder.+=(newTree)
    } else {
      // end of doc
      rootTree = newTree
      _depth -= 1
    }
  }

  def beginSub( elem : Elem, builder : => XmlBuilder) {
    _depth += 1
    
    if (_depth == _proxies.length) {
      // double the size
      val ar = Array.ofDim[TreeProxy]( _proxies.length * 2 )
      Array.copy(_proxies, 0, ar, 0, _proxies.length)
      _proxies = ar
    }

    if (_depth == _size) {
      _current = new TreeProxy(elem, builder)
      _proxies(_depth) = _current
      _size +=1
    } else {
      _current = _proxies(_depth)
      _current.setElem(elem)
      _current.builder.clear() // don't create a new one
    }
  }

  /**
   * Only call when its the end of the parse
   */ 
  def tree = 
    rootTree

}
