package scales.xml

import scales.utils._

/**
 * Iteratees related to pull parsing
 */ 
trait PullIteratees {

  /**
   * Wraps XmlPull
   */ 
  def iterate(path: List[QName], xml: XmlPull): FlatMapIterator[String] = iterate(path, xml.it)

  class Iterate(path: List[QName], xml: Iterator[PullType]) extends FlatMapIterator[String] { self =>
    var hasonce = false
    def hasNext = !hasonce
    def next = {
      if (!hasonce) {
	  hasonce = true
      }
      "fred"
    }
        
  }

  /**
   * A wrapping around withIter(onDone(List(onQNames(path))))(enumXml(xml, _))
   * it unwraps the data providing an Iterator[XPath]
   */
  def iterate(path: List[QName], xml: Iterator[PullType]): FlatMapIterator[String] =
    new Iterate(path, xml)

}
