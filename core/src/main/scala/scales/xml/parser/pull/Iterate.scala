package scales.xml.parser.pull


import scalaz.Equal, scalaz.Equal._, scalaz.Scalaz._

import scales.utils._

import scales.xml.{
  PullType,
  noXmlPath,
  XmlPath,
  QName,
  ScalesXml,
  Elem, EndElem,
  XmlItem,
  addAndFocus,
  addChild,
  XmlBuilder,
  XmlPull,
  parser,
  impl => ximpl
  }

import collection.FlatMapIterator

trait IterateFunctions {
  /**
   * Wraps XmlPull using default QName equality
   */
  def iterate(path: List[QName], xml: XmlPull): FlatMapIterator[XmlPath] = iterate(path, xml.it)

  /**
   * Provides an optimised version of withIter(onDone(List(onQNames(path))))(enumXml(xml, _)) with default QName equality
   * it unwraps the data providing an Iterator[XPath]
   */
  def iterate(path: List[QName], xml: Iterator[PullType]): FlatMapIterator[XmlPath] =
    new Iterate(path, xml)

  /**
   * Wraps XmlPull with an implicit parameter for QName equality
   */
  def iterateI(path: List[QName], xml: XmlPull)(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] = iterateI(path, xml.it)(qe)

  /**
   * Provides an optimised version of withIter(onDone(List(onQNames(path))))(enumXml(xml, _)) with an implicit parameter for QName equality 
   * it unwraps the data providing an Iterator[XPath]
   */
  def iterateI(path: List[QName], xml: Iterator[PullType])(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] =
    new Iterate(path, xml, qe)

}


/**
 * Iterates over a path of QNames producing XPaths for a given Iterator[PullType]
 */
class Iterate(path: List[QName], xml: Iterator[PullType], qe: Equal[QName] = ScalesXml.qnameEqual) extends FlatMapIterator[XmlPath] {
  import ScalesXml.{qnameEqual => _, _}
  import ScalesUtils._
  import ximpl.TreeProxies
  val qnames = path
  implicit val iqe = qe

  if (qnames.isEmpty) error("QNames is empty")

  /* see onQName for implementation basis */

  var before: List[(QName, Int)] = _
  var focus: (QName, Int) = _
  var toGo: List[(QName, Int)] = _
  var proxies: TreeProxies = new TreeProxies()
  var collecting: Boolean = _

  def reset {
    set(Nil, (qnames.head, 0), qnames.tail.map((_,0)),
	proxies.reuse, false)
  }

  reset

  def set(before: List[(QName, Int)], focus: (QName, Int), toGo: List[(QName, Int)], proxies: TreeProxies, collecting: Boolean) {
    this.before = before
    this.focus = focus
    this.toGo = toGo
    this.proxies = proxies
    this.collecting = collecting
  }

  def getNext = step
  var cur = getNext

  def hasNext = cur ne null
  def next = {
    val t = cur
    cur = getNext
    t
  }

  def step : XmlPath = {
    var res : XmlPath = null.asInstanceOf[XmlPath]
    while(xml.hasNext && res == null) {
      val e = xml.next
      e match {

	case Left(elem@Elem(q, a, n)) => {
	  val nfocus =
	    if (q === focus._1) (focus._1, focus._2 + 1)
	    else focus

	  proxies.beginSub(elem, XmlBuilder())
	  //val npath = addAndFocus(path, elem)

	  val shouldCollect = collecting || (toGo.isEmpty && q === focus._1)

	  // is it our head?
	  if ((!toGo.isEmpty) && q === focus._1)
	    // move down
	    set(before :+ focus, toGo.head, toGo.tail, proxies, false)
	  else
	    // wait for a down
	    set(before, nfocus, toGo, proxies, shouldCollect)
	}

	case Left(x: XmlItem) =>
	  if (collecting) {// collect
	    //addChild(path, x)
	    proxies.addChild(x)
	    set(before, focus, toGo, proxies, true)
	  }
	  else
	    set(before, focus, toGo, proxies, false) // don't collect

	case Right(EndElem(q, n)) =>

	  if (q === focus._1) {
	    val ncfocus = (focus._1, focus._2 - 1)

	      if (toGo.isEmpty && ncfocus._2 == 0) { // we are popping to the selected level
		res = proxies.proxyPath
		set(before, ncfocus, toGo,
		    // remove all children on the next iteration path.removeAndUp.getOrElse(noXmlPath)
		    proxies, false)
	      }
	      else {
		if (before.isEmpty)
		  reset // only when the root is asked for, could just refuse that of course?
		else {
		  if (collecting) {
		    // we are collecting but we still have more than 0 repeated qnames deep
		    proxies.elementEnd() // path.zipUp
		    set(before, ncfocus, toGo, proxies, true)
		  }
		  else {
		    // we aren't collecting but we are moving up, we just have repeated names
		    val nfocus = before.last
		    val nbefore = before.dropRight(1)
		    set(nbefore, nfocus, focus :: toGo, // removeand
			proxies.proxyRemoveAndUp(), false // we have NOT been collecting
		      )
		  }
		}
	      }
	  } else {
	    set(before, focus, toGo,
		if (collecting) { // empty is not enough, it should also be definitely collecting
		  //path.zipUp
		  proxies.elementEnd
		  proxies
		} else
		  proxies.proxyRemoveAndUp(), collecting)
	  }

      }
    }

    res
  }



  /*
   val orig = withIter(xml)(onQNames(path))
   def getNext = {
   if (orig.hasNext) {
   val t = orig.next
   if (t._2.isDefined)
   (true, t._2)
   else (false, None)
   } else (false, None)
   }
   var cur = getNext
   def hasNext = cur._1 && cur._2.isDefined
   def next = {
   val t = cur._2
   cur = getNext
   t.get
   }
   */
}
