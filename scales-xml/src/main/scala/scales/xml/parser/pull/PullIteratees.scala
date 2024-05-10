package scales.xml.parser.pull

import scales.utils._
import scales.xml.{Elem, EndElem, PullType, QName, ScalesXml, XmlBuilder, XmlItem, XmlPath, addAndFocus, addChild, noXmlPath, impl => ximpl}
import collection.FlatMapIterator
import scalaz.Free.Trampoline
import scalaz.Id.Id
import scalaz.Scalaz.ToEqualOps
import scalaz.effect.IO
import scalaz.{Equal, Monad}
import scalaz.iteratee.Input
import scalaz.iteratee.Input.{Empty, Eof}
import scalaz.iteratee.Iteratee.iterateeT
import scalaz.iteratee.{IterateeT, StepT}
import scalaz.iteratee.StepT.{Cont, Done}

import scales.utils.iteratee.functions._

class PullIterateeFunctions[F[_]](val F: Monad[F]){
  import scales.xml.{QNamesMatch, PeekMatch}

  def onQNames(qnames: List[QName])(implicit F: Monad[F]): ResumableIter[PullType, F, QNamesMatch] =
    PullIteratees.onQNames[F](qnames)

  /**
   * Collects all data belonging to an element that matches
   * the list. <top><middle><ofInterest> content </ofInterest><ofInterest....
   * onQNames(List("top"l, "middle"l, "ofInterest"l))
   * would return an iteratee that returned every <ofInterest> content </ofInterest>
   * as a path (each parent node containing only one child node).
   */
  def onQNamesI(qnames: List[QName])(implicit qe: Equal[QName], F: Monad[F]): ResumableIter[PullType, F, QNamesMatch] =
    PullIteratees.onQNamesI[F](qnames)

  def skipv(downTo: Int*)(implicit F: Monad[F]): IterateeT[PullType, F, PeekMatch] =
    PullIteratees.skipv[F](downTo: _*)

  /**
   * Skips all events until the indexes match downTo, can be seen as
   * \*\*[b]\*[c] skipping until c with skip(List(b,c)).
   * This can be used, for example, to identify qnames within a message and combined with capture to allow replaying.
   * Identifying a soap doc-lit request would be skip(List(2,1)).
   * It returns the XmlPath to the skipped position, for soap /Envelope/Body/Request but does not collect the contents of that node.
   * An empty list will simply return the first Element found.
   */
  def skip(downTo: => List[Int])(implicit F: Monad[F]): IterateeT[PullType, F, PeekMatch] =
    PullIteratees.skip[F](downTo)

}

/**
 * Iteratees related to pull parsing
 */
trait PullIteratees {

  def pullIterateesOf[F[_]](implicit F: Monad[F]): PullIterateeFunctions[F] = new PullIterateeFunctions[F](F)

  implicit val ioPullIteratees = pullIterateesOf[IO]
  implicit val trampolinePullIteratees = pullIterateesOf[Trampoline]
  // not recommended but may help migrations
  implicit val idPullIteratees = pullIterateesOf[Id]

  type PeekMatch = PullIteratees.PeekMatch

  type QNamesMatch = PullIteratees.QNamesMatch

  /**
   * Wraps XmlPull
   */
  def iterate(path: List[QName], xml: XmlPull)(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] = iterate(path, xml.it)

  /**
   * A wrapping around withIter(onDone(List(onQNames(path))))(enumXml(xml, _))
   * it unwraps the data providing an Iterator[XPath]
   */
  def iterate(path: List[QName], xml: Iterator[PullType])(implicit qe: Equal[QName]): FlatMapIterator[XmlPath] =
    new Iterate(path, xml)

}

object PullIteratees {

  type QNamesMatch = (List[QName], Option[XmlPath])
  /**
   * Collects all data belonging to an element that matches
   * the list. <top><middle><ofInterest> content </ofInterest><ofInterest....
   * onQNames(List("top"l, "middle"l, "ofInterest"l))
   * would return an iteratee that returned every <ofInterest> content </ofInterest>
   * as a path (each parent node containing only one child node).
   */
  def onQNames[F[_]: Monad](qnames: List[QName]): ResumableIter[PullType, F, QNamesMatch] = onQNamesI(qnames)(ScalesXml.qnameEqual, implicitly[Monad[F]])

  /**
   * Collects all data belonging to an element that matches
   * the list. <top><middle><ofInterest> content </ofInterest><ofInterest....
   * onQNames(List("top"l, "middle"l, "ofInterest"l))
   * would return an iteratee that returned every <ofInterest> content </ofInterest>
   * as a path (each parent node containing only one child node).
   *
   * This version of onQNamesI allows a custom QName Equal to be passed.
   */
  def onQNamesI[F[_]](qnames: List[QName])(implicit qe: Equal[QName], F: Monad[F]): ResumableIter[PullType, F, QNamesMatch] = {

    /*
     * The pairs allow the depth of each element to be followed.  In particular this stops both descent and ascent problems in the
     * pushing and popping on the stack.  I.e. it covers the case where you have nested repeating QNames, both when you are looking for them
     * and when your are not.  Don't pop too early and don't incorrectly force a done.

    lazy val starter = Cont(step(Nil, (qnames.head, 0), qnames.tail.map((_, 0)), noXmlPath, false))

    def step(before: List[(QName, Int)], focus: (QName, Int), toGo: List[(QName, Int)], path: XmlPath, collecting: Boolean)(s: Input[PullType]): ResumableIter[PullType, F, QNamesMatch] = {
      iterateeT( Monad[F].point(
        s(el = {
            case Left(elem@Elem(q, a, n)) => {
              val nfocus =
                if (q === focus._1)
                  (focus._1, focus._2 + 1)
                else
                  focus

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
                    iterateeT( Monad[F].point( Cont(step(before, ncfocus, toGo,
                      // remove all children on the next iteration
                      path.removeAndUp.getOrElse(noXmlPath), false))))), Empty[PullType])
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

          },
          empty = Cont(step(before, focus, toGo, path, false)),
          eof = Done(((qnames, None), iterateeT( Monad[F].point(starter) )), Eof[PullType])
        )
      ))
    }
     */

    lazy val starter = Cont(step(Nil, noXmlPath, false))

    def step(before: List[QName], path: XmlPath, collecting: Boolean)(s: Input[PullType]): ResumableIter[PullType, F, QNamesMatch] = {
      iterateeT( Monad[F].point(
        s(el = {
          case Left(elem@Elem(q, a, n)) => {
            val npath = addAndFocus(path, elem)

            val shouldCollect = collecting || (
              before.size == (qnames.size - 1) &&
              // eval only when needed
              (before :+ q).zip(qnames).forall(p => p._1 === p._2)
            )

            Cont( step(before :+ q, npath, shouldCollect) )
          }

          case Left(x: XmlItem) =>
            Cont(step(before,
              if (collecting)
                addChild(path, x)
              else
                path, collecting))

          case Right(EndElem(q, n)) =>
            // is this the end path ?  We have to re-verify the path, do so only when needed but stops same height (but not same path)
            // and nested repeat issues
            val haveCollected = collecting && before.size == qnames.size && before.zip(qnames).forall(p => p._1 === p._2)
            val npath =
              if (haveCollected) {
                // done with path
                path
              } else
                if (collecting) {
                  // we are popping to the selected level
                  path.zipUp()
                } else {
                  path.removeAndUp().getOrElse(noXmlPath)
                }

            if (haveCollected)
              Done(((qnames, Some(npath)),
                iterateeT( Monad[F].point( Cont(step(before.dropRight(1),
                  // remove all children on the next iteration
                  npath.removeAndUp.getOrElse(noXmlPath), collecting && !haveCollected))))), Empty[PullType])
            else
              Cont(step(before.dropRight(1), npath, collecting && !haveCollected))

          },
          empty = Cont(step(before, path, false)),
          eof = Done(((qnames, None), iterateeT( Monad[F].point(starter) )), Eof[PullType])
        )
      ))
    }

    if (qnames.isEmpty) error("Qnames is empty")

    iterateeT( Monad[F].point(starter ))
  }

  type PeekMatch = Option[XmlPath]

  def skipv[F[_]](downTo: Int*)(implicit F: Monad[F]): IterateeT[PullType, F, PeekMatch] = skip[F](List(downTo: _*))

  /**
   * Skips all events until the indexes match downTo, can be seen as
   * \*\*[b]\*[c] skipping until c with skip(List(b,c)).
   * This can be used, for example, to identify qnames within a message and combined with capture to allow replaying.
   * Identifying a soap doc-lit request would be skip(List(2,1)).
   * It returns the XmlPath to the skipped position, for soap /Envelope/Body/Request but does not collect the contents of that node.
   * An empty list will simply return the first Element found.
   */
  def skip[F[_]](downTo: => List[Int])(implicit F: Monad[F]): IterateeT[PullType, F, PeekMatch] = {

    lazy val dEof: StepT[PullType, F, PeekMatch] = Done(None, Eof[PullType])

    def step(before: List[Int], pos: List[Int], toGo: List[Int], path: XmlPath)(s: Input[PullType]): IterateeT[PullType, F, PeekMatch] =
      iterateeT(F.point(
        s(el = {

            case Left(elem@Elem(q, a, n)) => {
              lazy val npath = addAndFocus(path, elem)
              val npos = pos.head + 1 :: pos.tail
              val could = toGo.head == npos.head
              //println("pos "+pos+ " npos "+npos+" before "+before+" toGo "+toGo)
              if (pos.size == (before.size + 1)) // correct level
                if (toGo.size == 1 && could)
                  Done(Some(npath), Empty[PullType])
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

          },
          empty = Cont(step(before, pos, toGo, path)),
          eof = dEof //Done((downTo, None),IterV.EOF[PullType])
          )
      ))

    iterateeT( F.point( Cont(step(List[Int](), List(0), 1 :: downTo, noXmlPath)) ) )
  }

}

/**
 * Iterates over a path of QNames producing XPaths for a given Iterator[PullType]
 */
class Iterate(path: List[QName], xml: Iterator[PullType])(implicit qe: Equal[QName]) extends FlatMapIterator[XmlPath] {
  import ScalesXml.{qnameEqual => _, _}
  import ScalesUtils._
  import ximpl.TreeProxies
  val qnames = path

  if (qnames.isEmpty) error("QNames is empty")

  /* see onQName for implementation basis */

  var before: List[QName] = _
  var proxies: TreeProxies = new TreeProxies()
  var collecting: Boolean = _

  def reset {
    set(Nil, proxies.reuse, false)
  }

  reset

  def set(before: List[QName], proxies: TreeProxies, collecting: Boolean) {
    this.before = before
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

        case Left(elem@Elem(q, a, n)) =>
          proxies.beginSub(elem, XmlBuilder())

          val shouldCollect = collecting || (
            before.size == (qnames.size - 1) &&
            // eval only when needed
            (before :+ q).zip(qnames.dropRight(1)).forall(p => p._1 === p._2)
          )
          set(before :+ q, proxies, shouldCollect)

        case Left(x: XmlItem) =>
          if (collecting) {
            proxies.addChild(x)
          }
          set(before, proxies, collecting)

        case Right(EndElem(q, n)) =>

          // is this the end path ?  We have to re-verify the path, do so only when needed but stops same height (but not same path)
          // and nested repeat issues
          val haveCollected = collecting && before.size == qnames.size && before.zip(qnames).forall(p => p._1 === p._2)
          if (haveCollected) {
            res = proxies.proxyPath
          } else
            if (collecting) {
              // we are popping to the selected level
              proxies.elementEnd()
            } else {
              proxies.proxyRemoveAndUp()
            }

          set(before.dropRight(1), proxies, collecting && !haveCollected)
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
