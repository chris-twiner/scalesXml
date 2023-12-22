package scales.xml.trax

import scales.xml._

import scales.utils.top
import scales.utils.collection.path.AbstractPathIterator

class TreeIterable( tree : XmlTree ) extends AbstractPathIterator[XmlItem, Elem, XCC, PullType] {
  lazy val initialPath = top(tree)

  def event : PullType = path.node.focus.fold(x=>x,y=>y.section)

  def end = {
    val el = path.tree.section
    EndElem(el.name, el.namespaces) : PullType
  }
 
}
