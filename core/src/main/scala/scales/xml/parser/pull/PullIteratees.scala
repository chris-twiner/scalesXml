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
  parser,
  impl => ximpl
  }

import scales.xml.parser.strategies.{MemoryOptimisationStrategy, OptimisationToken}

import collection.FlatMapIterator

/**
 * Iteratees related to pull parsing
 */
trait PullIteratees {

  // enumerators and iteratees follow

  import scalaz.{IterV, Enumerator, Input, EphemeralStream}
  import scalaz.IterV._

  type QNamesMatch = (List[QName], Option[XmlPath])

  /**
   * Collects all data belonging to an element that matches
   * the list. <top><middle><ofInterest> content </ofInterest><ofInterest....
   * onQNames(List("top"l, "middle"l, "ofInterest"l))
   * would return an iteratee that returned every <ofInterest> content </ofInterest>
   * as a path (each parent node containing only one child node).
   */
  def onQNames(qnames: List[QName])(implicit qe: Equal[QName]): ResumableIter[PullType, QNamesMatch] = {

    /*
     * The pairs allow the depth of each element to be followed.  In particular this stops both descent and ascent problems in the
     * pushing and popping on the stack.  I.e. it covers the case where you have nested repeating QNames, both when you are looking for them
     * and when your are not.  Don't pop to early and don't incorrectly force a done.
     */

    lazy val starter = Cont(step(Nil, (qnames.head, 0), qnames.tail.map((_, 0)), noXmlPath, false))

    def step(before: List[(QName, Int)], focus: (QName, Int), toGo: List[(QName, Int)], path: XmlPath, collecting: Boolean)(s: Input[PullType]): ResumableIter[PullType, QNamesMatch] =
      s(el = { e => //println(e +" "+before+" "+focus+" " + toGo);
        e match {

          case Left(elem@Elem(q, a, n)) => {
            val nfocus = if (q === focus._1) (focus._1, focus._2 + 1)
            else focus
            val npath = addAndFocus(path, elem)

            val shouldCollect = collecting || (toGo.isEmpty && q === focus._1)

            Cont(
              // is it our head?
              if ((!toGo.isEmpty) && q === focus._1)
                // move down
                step(before :+ focus, toGo.head, toGo.tail, npath, false)
              else
                // wait for a down
                step(before, nfocus, toGo, npath, shouldCollect))
          }

          case Left(x: XmlItem) =>
            if (collecting) // collect
              Cont(step(before, focus, toGo, addChild(path, x), true))
            else
              Cont(step(before, focus, toGo, path, false)) // don't collect

          case Right(EndElem(q, n)) =>

            if (q === focus._1) {
              val ncfocus = (focus._1, focus._2 - 1)

              if (toGo.isEmpty && ncfocus._2 == 0) // we are popping to the selected level
                Done(((qnames, Some(path)),
                  Cont(step(before, ncfocus, toGo,
                    // remove all children on the next iteration
                    path.removeAndUp.getOrElse(noXmlPath), false))), IterV.Empty[PullType])
              else {
                if (before.isEmpty)
                  starter // only when the root is asked for, could just refuse that of course?
                else {
                  if (collecting)
                    // we are collecting but we still have more than 0 repeated qnames deep
                    Cont(step(before, ncfocus, toGo, path.zipUp, true))
                  else {
                    // we aren't collecting but we are moving up, we just have repeated names
                    val nfocus = before.last
                    val nbefore = before.dropRight(1)
                    Cont(step(nbefore, nfocus, focus :: toGo,
                      path.removeAndUp.getOrElse(noXmlPath), false // we have NOT been collecting
                      ))
                  }
                }
              }
            } else {
              Cont(step(before, focus, toGo,
                if (collecting) // empty is not enough, it should also be definitely collecting
                  path.zipUp
                else
                  path.removeAndUp.getOrElse(noXmlPath), collecting))
            }

        }
      },
        empty = Cont(step(before, focus, toGo, path, false)),
        eof = Done(((qnames, None), starter), IterV.EOF[PullType]))

    if (qnames.isEmpty) error("Qnames is empty")

    starter
  }

  type PeekMatch = Option[XmlPath]

  def skipv(downTo: Int*): IterV[PullType, PeekMatch] = skip(List(downTo: _*))

  /**
   * Skips all events until the indexes match downTo, can be seen as
   * \*\*[b]\*[c] skipping until c with skip(List(b,c)).
   * This can be used, for example, to identify qnames within a message and combined with capture to allow replaying.
   * Identifying a soap doc-lit request would be skip(List(2,1)).
   * It returns the XmlPath to the skipped position, for soap /Envelope/Body/Request but does not collect the contents of that node.
   * An empty list will simply return the first Element found.
   */
  def skip(downTo: => List[Int]): IterV[PullType, PeekMatch] = {

    lazy val dEof: IterV[PullType, PeekMatch] = Done(None, IterV.EOF[PullType])

    def step(before: List[Int], pos: List[Int], toGo: List[Int], path: XmlPath)(s: Input[PullType]): IterV[PullType, PeekMatch] =
      s(el = { e =>
        e match {

          case Left(elem@Elem(q, a, n)) => {
            lazy val npath = addAndFocus(path, elem)
            val npos = pos.head + 1 :: pos.tail
            val could = toGo.head == npos.head
            //println("pos "+pos+ " npos "+npos+" before "+before+" toGo "+toGo)
            if (pos.size == (before.size + 1)) // correct level
              if (toGo.size == 1 && could)
                Done(Some(npath), IterV.Empty[PullType])
              else if (npos.head > toGo.head)
                dEof
              else if (could)
                // pop and move down
                Cont(step(before :+ toGo.head, 0 :: npos, toGo.tail, npath))
              else
                Cont(step(before, 0 :: npos, toGo, npath))
            else
              Cont(step(before, 0 :: npos, toGo, npath))

          }

          // just return this again
          case Left(x: XmlItem) =>
            Cont(step(before, pos, toGo, path))

          // pop up no collecting, loose the head as we are moving up again
          case Right(EndElem(q, n)) =>
            // get or else end doc elem
            if (pos.size > 0 && pos.size == before.size + 1)
              // we have moved down in toGo
              Cont(step(before.dropRight(1), pos.tail, before.last :: toGo, path.removeAndUp().getOrElse(noXmlPath)))
            else
              Cont(step(before, pos.tail, toGo, path.removeAndUp().getOrElse(noXmlPath)))

        }
      },
        empty = Cont(step(before, pos, toGo, path)),
        eof = dEof //Done((downTo, None),IterV.EOF[PullType])
        )

    Cont(step(List[Int](), List(0), 1 :: downTo, noXmlPath))
  }

  /**
   * Wraps XmlPull
   */
  def iterate(path: List[QName], xml: XmlPull)(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] = iterate(path, xml.it)(qe)

  /**
   * A wrapping around withIter(onDone(List(onQNames(path))))(enumXml(xml, _))
   * it unwraps the data providing an Iterator[XPath]
   */
  def iterate(path: List[QName], xml: Iterator[PullType])(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] =
    new Iterate(path, xml)(qe)

}

/**
 * Iterates over a path of QNames producing XPaths for a given Iterator[PullType]
 */
class Iterate(path: List[QName], xml: Iterator[PullType])(implicit qe:Equal[QName]) extends FlatMapIterator[XmlPath] {
  import ScalesXml.{qnameEqual => _, _}
  import ScalesUtils._
  import ximpl.TreeProxies
  val qnames = path

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
