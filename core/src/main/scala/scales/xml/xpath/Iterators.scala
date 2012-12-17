package scales.xml.xpath

import scales.utils._
import collection.path.DirectionIterator
import scales.xml._

/**
 * Returns the ancestors / parents of the path
 */ 
class Ancestors( val initial : XmlPath ) extends Iterable[XmlPath] {
  class ParentIterator( var path : XmlPath) extends Iterator[XmlPath] {
    def hasNext = path.top.isRight
    def next = {
      path = path.top.getRight
      path
    }    
  }
  def iterator = new ParentIterator(initial)
}

/**
 * Iterates over paths using the document order as per the following_:: axis.
 * 
 */
class Following( val initial : XmlPath ) extends Iterable[XmlPath] {
  
  protected val f = following(initial)

  def iterator = f.map( (x : XmlPath) => new DirectionIterator[XmlItem, Elem, XCC](x, true)).getOrElse(Nil.iterator)
}

/**
 * Iterates over paths using reverse document order and, as per the preceding_:: axis, skips all ancestors.
 */
class Preceding( val initial : XmlPath ) extends Iterable[XmlPath] {
  
  protected val p = preceding(initial)

  /**
   * Sets aren't safe here. We must use the original field not the preceding
   */ 
  protected val parents = new Ancestors(initial)

  def iterator = p.map( (x : XmlPath) => new DirectionIterator[XmlItem, Elem, XCC](x, false).
    filterNot( i => parents.exists( t => comparePathsDirect(i,t) ) ) ).
    getOrElse(Nil.iterator)
}
