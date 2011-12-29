package scales.utils

import scala.collection.IndexedSeqLike

/**
 * Provides an Iterator from a given initial path that traverses the entire tree, allows both forwards and backwards iteration.
 *
 * It specifically provides an up event, allowing consumers to "pop" the element stack
 */ 
trait AbstractPathIterator[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], T] extends Iterator[T] {

  val initialPath : Path[Item, Section, CC]

  var path = initialPath

  val isForward = true

  sealed trait State {
    def hasNext : Boolean = true
    def next : (State, T)
  }
  
  case object StartElem extends State {
    // always has the start
    def next = {
//println("doing StartElem from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))	
      val ev = event
      // turn to firstChild map getOrElse  
      if (path.hasChildren) {
//println("doing StartElem hasChildren from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))	
	if (isForward)
	  path = path.firstChild.get
	else {
	  path = deepestLast(path)
	}
//println("doing StartElem now "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))	

	if (path.isItem)
	  (OnItem, ev)
	else
	  (StartElem, ev)
      } else
	(EndElemS, ev)
    }
  }

  final def canDoNext = 
    if (isForward)
      path.hasNextSibling
    else
      path.hasPreviousSibling

  case object OnItem extends State {
    // always either nextSibling or EndElem
    def next = {
      val ev = event

      if (canDoNext)
	doNext(ev)
      else {
	// direct zip Up
	path = path.top.right.get
	(EndElemS, ev)
      }
    }
  }

  case object End extends State {
    override def hasNext = false
    def next = error("Can't go past the root")
  }

  def end : T

  case object EndElemS extends State {
    // we will always have at least one last EndElemS before End
    def next = {
//println("doing EndElemS next from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))
      val ev = end
      if (canDoNext) {
//println("doing EndElemS says we can do next from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))	
	// element without children
	doNext(ev)
      } else {
	if (path.top.isLeft) {
	  (End, ev) // last event
	} else {
	  // direct zip Up
	  path = path.top.right.get
	  (EndElemS, ev)	  
	}
      }
    }
  }

  def doNext(ev : T) : (State, T) = {
    path = 
      if (isForward)
	path.nextSibling
      else {
//	println("doing doNext before from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))
	val t = path.previousSibling
	if (t.isItem)
	  t
	else
	  deepestLast(t)
      }
//    println("doing doNext after from "+ (if (!path.isItem) scales.xml.elem(path.asInstanceOf[scales.xml.XmlPath]).name.pqName else "" ))

    if ( path.isItem ) 
      (OnItem, ev)
    else
      (StartElem, ev)
  }

  def event : T

  // start it off depending on what path points to
  var nextState : State = initialPath.node.focus.fold(_ => OnItem, _ => StartElem)

  /**
   * called after nextState.next, allows filtering to prepare for the next event
   */ 
  def prepareNext {
  }

  def hasNext = nextState.hasNext
  def next = {
    val (ns, ev) = nextState.next
    nextState = ns
    prepareNext
    ev
  }
}

/**
 * Iterates over paths using the document order it skips over EndElemS events when going forward and StartElem when reversing, returning just the path.
 * Developers should call preceding or following before entering this iterator.
 */
class DirectionIterator[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]]( val initialPath : Path[Item,Section,CC], override val isForward : Boolean = true ) extends AbstractPathIterator[Item, Section, CC, Path[Item,Section,CC]] {
  def event = path
  def end = path

  override def prepareNext {
    // the end element is actually the interesting one for us and we
    // skip the StartElem
    
    val donext = 
      (isForward && (nextState eq EndElemS)) ||
      (!isForward && (nextState eq StartElem))
    
    if (donext) {
      //println("doing next from "+ (if (!path.isItem) elem(path).name.pqName else "" ))
      next
    }
    
  }
}
