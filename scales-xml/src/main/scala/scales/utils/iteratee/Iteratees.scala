package scales.utils.iteratee

import scalaz.Free.Trampoline
import scalaz.Id.Id
import scalaz.effect.IO
import scalaz.{Applicative, Bind, EphemeralStream, Monad}
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.Iteratee.{cont, done, elInput, empty, foldM, iterateeT, repeat}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{EnumeratorT, Input, IterateeT, StepT}
import scales.utils.ScalesUtils

object EphemeralStreamEnum {

  /**
   * Enumerates over an EphemeralStream
   * @param xs
   * @tparam E
   * @tparam F
   * @return
   */
  def enumEphemeralStream[E, F[_] : Monad](xs: EphemeralStream[E]): EnumeratorT[E, F] = {
    import EphemeralStream.##::

    new EnumeratorT[E, F] {
      def apply[A] = (s: StepT[E, F, A]) =>
        xs match {
          case h ##:: t => s.mapCont(k => k(scalaz.iteratee.Iteratee.elInput(h)) >>== enumEphemeralStream[E, F](t).apply[A])
          case _ => s.pointI
        }

    }
  }

  /**
   * Enumerates over an EphemeralStream but allows progress to be "saved" between calls
   * @param state
   * @param xs
   * @tparam E
   * @tparam F
   * @return
   */
  def enumEphemeralStreamF[E, F[_] : Monad](state: EphemeralStream[E] => Unit)(xs: EphemeralStream[E]): EnumeratorT[E, F] = {
    import EphemeralStream.##::

    new EnumeratorT[E, F] {
      def apply[A] = (s: StepT[E, F, A]) =>
        xs match {
          case h ##:: t => s.mapCont(k => k(scalaz.iteratee.Iteratee.elInput(h)) >>== enumEphemeralStreamF[E, F]({state(t);state})(t).apply[A])
          case _ => s.pointI
        }

    }
  }

  /**
   * Converts an iterator to EphemeralStream.  EphemeralStream then ensures next will not be called unexpectedly in the face of trampolining
   * @param iterator
   * @tparam A
   * @return
   */
  def toEphemeral[A](iterator: Iterator[A]): EphemeralStream[A] =
    if (iterator.isEmpty) EphemeralStream.emptyEphemeralStream else {
      val next = iterator.next()
      val after = toEphemeral(iterator)
      EphemeralStream.cons(next, after)
    }
}

/**
 * Evals once, the developer must check its Done, equivalent to a .run but
 * doesn't lose the continuation - no "Diverging Iteratee"
 */
trait Eval[WHAT, F[_],RETURN] {

  val orig : IterateeT[WHAT, F, RETURN]

  def eval(implicit F: Monad[F]) : IterateeT[WHAT, F, RETURN] =
    iterateeT(
      F.bind((orig &= empty[WHAT,F]).value)((s: StepT[WHAT, F, RETURN]) => s.fold(
        cont = k => k(Eof[WHAT]).value
        , done = (a, i) => F.point(Done(a, i))
      )))
}

trait IterateeImplicits {
  implicit def toEval[WHAT, F[_], RETURN]( i : IterateeT[WHAT, F, RETURN] ) = new Eval[WHAT, F, RETURN] {
    lazy val orig = i
  }

}

/**
 * Collection of iterateees
 */
trait Iteratees {

  /**
   * Taken from huynhjl's answer on StackOverflow, just abstracting the type to allow for better implicit inference
   *
   * def iteratorEnumerator[E](iter: Iterator[E]) = new EnumeratorT[E, Trampoline] {
   * override def apply[A]: StepT[E, Trampoline, A] => IterateeT[E, Trampoline, A] =
   * {
   * case step if iter.isEmpty => iterateeT[E, Trampoline, A](Free.point(step))
   * case step @ Done(acc, input) => iterateeT[E, Trampoline, A](Free.point(step))
   * case step @ Cont(k) =>
   * val x : E = iter.next
   *
   * k(Element(x)) >>== {
   * s => s.mapContOr(_ => sys.error("diverging iteratee"), apply(s))
   * }
   * }
   * }
   */
  def iteratorEnumerator[E, F[_]](iter: Iterator[E])(implicit f: Monad[F]): EnumeratorT[E, F] =
    new EnumeratorT[E, F] {
      def apply[A] = {
        def go(xs: Iterator[E])(s: StepT[E, F, A]): IterateeT[E, F, A] =
          if (xs.isEmpty) s.pointI
          else {
            s mapCont { k =>
              val next = xs.next()
              k(elInput(next)) >>== go(xs)
            }
          }

        go(iter)
      }
    }

  def error(string: String) = sys.error(string)

  /** drop while iteratee, returning the possibly remaining data */
  def dropWhile[E, F[_]](f: (E) => Boolean)(implicit F: Monad[F]) : IterateeT[E, F, Option[E]] =
    dropWhileM[E,F]{ e => F.point(f(e)) }

  /**
   * Same as dropWhile but captures the Monad in F
   */
  def dropWhileM[E, F[_]](f: (E) => F[Boolean])(implicit F: Monad[F]) : IterateeT[E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[E, F, Option[E]] =
      s(
        el = e => {
          iterateeT(F.map(f(e)) {
            shouldCont =>
              if (shouldCont)
                Cont(step)
              else
                Done(Some(e), Empty[E])
          })
        },
        empty = Cont(step).pointI,
        eof = Done[E,F,Option[E]](None, Eof[E]).pointI
      )

    iterateeT( F.point( Cont(step) ) )
  }

  /** "find" iteratee, finds Some(first) or None */
  def find[E, F[_]](f: (E) => Boolean)(implicit F: Monad[F]) : IterateeT[E, F, Option[E]] =
    dropWhile[E,F](!f(_))

  /** filter iteratee, greedily taking all content until eof */
  def filter[E, F[_]](f: (E) => Boolean)(implicit F: Monad[F]): IterateeT[E, F, Iterable[E]] = {
    def step(l : List[E])(s: Input[E]): IterateeT[E, F, Iterable[E]] =
      iterateeT( F.point( s(el = e => {
        if (f(e))
          Cont(step(l :+ e))
        else
          Cont(step(l))
        },
        empty = Cont(step(l)),
        eof = Done(l, Eof[E])) ) )

    iterateeT( F.point( Cont(step(List())) ) )
  }

  type ResumableIterList[E, F[_], A] = IterateeT[E, F, (Iterable[A],IterateeT[E,F,_])]
  type ResumableIterListStep[E, F[_], A] = StepT[E, F, (Iterable[A],IterateeT[E,F,_])]
  type ResumableIter[E, F[_], A] = IterateeT[E, F, (A, IterateeT[E, F,_])]
  type ResumableStep[E, F[_], A] = StepT[E, F, (A, IterateeT[E, F, _])]

  /**
   * marks a continuation resumableiter as actually being EOF - i.e. don't attempt to evaluate it
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @return
   */
  def resumableEOF[E, F[_], A](input: A = null)(implicit F: Applicative[F]): ResumableIter[E, F, A] =
    iterateeT(F.point( Done[E, F, (A, ResumableIter[E,F,A])]((input, null.asInstanceOf[ResumableIter[E,F,A]]), Eof[E]) )).asInstanceOf[ResumableIter[E, F, A]]

  def resumableEOFDone[E, F[_], A](input: A)(implicit F: Applicative[F]): ResumableStep[E, F, A] =
    Done[E, F, (A, ResumableIter[E, F, A])]((input, resumableEOF(input)), Eof[E]).asInstanceOf[ResumableStep[E,F,A]]

  /**
   * is this iteratee actually "empty"
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @return
   */
  def isResumableEOF[E, F[_], A](iter: ResumableIter[E,F,A])(implicit F: Monad[F]): F[Boolean] =
    F.map(iter.value)(isResumableEOFS[E,F,A])

  def isResumableEOFS[E, F[_], A](s: ResumableStep[E,F,A]): Boolean =
    s(
      cont= k => false
      , done= (a, i) => a._2 == null && i.isEof
    )

  /**
   * Extract the continuation from a Done
   */
  def extractCont[E, F[_], A]( iter : ResumableIter[E, F, A] )(implicit F: Monad[F]): ResumableIter[E, F, A] =
    iterateeT(
      F.bind(iter.value)(s => s.fold(
        cont = k => error("Was not done")
        , done = (x, i) => x._2.asInstanceOf[ResumableIter[E, F, A]].value
      )))

  /**
   * Extract the continuation from a Done
   */
  def extractContS[E, F[_], A]( s : ResumableStep[E, F, A] )(implicit F: Monad[F]): ResumableIter[E, F, A] =
    iterateeT( s(
        cont = k => error("Was not done")
        , done = (x, i) => x._2.asInstanceOf[ResumableIter[E, F, A]].value
      ) )

  /**
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  def extract[E, F[_], A]( iter : ResumableIter[E, F, A] )(implicit F: Monad[F]) : F[Option[A]] =
    iter.foldT[Option[A]](
      done = (x, i) =>
	      F.point(Some(x._1)),
      cont = f => F.point(None) )

  /**
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  def extractS[E, F[_], A]( iter : ResumableStep[E, F, A] )(implicit F: Monad[F]) : Option[A] =
    iter(
      done = (x, i) =>
        Some(x._1),
      cont = f => None )

  /**
   * Helper to identify dones
   */
  def isDone[E, F[_], A]( iter : IterateeT[E, F, A] )(implicit F: Monad[F]): F[Boolean] =
    F.map(iter.value)(isDoneS[E,F,A])

  def isDoneS[E, F[_], A]( step : StepT[E, F, A] )(implicit F: Monad[F]): Boolean =
    step(
      done = (a, i) => true,
      cont = f => false
    )

  /**
   * Helper for done and empty
   */
  def isEmpty[E,F[_],A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    F.map(iter.value)(isEmptyS[E,F,A])

  /**
   * Helper for done and eof
   */
  def isEOF[E, F[_], A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    F.map(iter.value)(isEOFS[E,F,A])

  /**
   * Helper for done and empty
   */
  def isEmptyS[E,F[_],A]( step : StepT[E,F,A] )(implicit F: Monad[F]): Boolean =
    step(
      done = (a, i) => i.isEmpty,
      cont = f => false
    )

  /**
   * Helper for done and eof
   */
  def isEOFS[E, F[_], A]( step : StepT[E,F,A] )(implicit F: Monad[F]): Boolean =
    step(
      done = (a, i) => i.isEof,
      cont = f => false
    )

  /**
   * Converts a normal IterV[E,A] to a ResumableIter.
   *
   * Does so by folding over the iter once for an input
   * and when its Done starting again
   * with the original iter.  This is close to restarting the iter on
   * a new "stream", otherwise all attempts to keep the Cont will be made.
   */
  implicit def toResumableIter[E,F[_],A]( oiter : IterateeT[E,F,A])(implicit F: Monad[F]) : ResumableIter[E,F,A] = {
    def step(iter : IterateeT[E,F,A])( s : Input[E]) : ResumableIter[E, F, A] = {
      val next = iter.foldT[ResumableStep[E,F,A]](// need to evaluate s in the case of done....
        done = (x, y) => F.point( Done((x, iterateeT(F.point(Cont(step(oiter))))),y) ),
        cont = k =>
          {
            k(s).foldT(
              done = (x, y) => F.point( Done((x, iterateeT(F.point(Cont(step(oiter))))),y) ),
              cont = i => F.point( Cont(step( iterateeT( F.point( Cont(i) )) )) )
            )
          }
      )
      iterateeT( next )
    }

    iterateeT( F.point( Cont(step(oiter)) ) )
  }

  /**
   * Stepwise fold, each element is evaluated but each one is returned as a result+resumable iter.
   */
  def foldI[E,F[_],A]( f : (E,A) => A )( init : A, stopOn: A => Boolean = (_: A) => true )(implicit F: Monad[F]) : ResumableIter[E,F,A] =
    foldIM[E,F,A]{ (e,a) => F.point(f(e,a)) }(init, stopOn)

  /**
   * Stepwise fold but the result of f is bound to F
   */
  def foldIM[E,F[_],A]( f : (E,A) => F[A] )( init : A, stopOn: A => Boolean = (_: A) => true )(implicit F: Monad[F]) : ResumableIter[E,F,A] = {
    def step( current : A )( s : Input[E] ) : IterateeT[E,F,(A, IterateeT[E,F,_])] =
      s(
        el = e =>
          IterateeT.IterateeTMonadTrans[E].liftM(f(e, current)) flatMap{ i =>
            if (!stopOn(i))
              cont(step(i))
            else
              done[E, F, (A, IterateeT[E, F, _])]((i, iterateeT(F.point(Cont(step(i))))), Empty[E])
          },
        empty = cont(step(current)),
        eof = done((current, iterateeT( F.point( Cont(step(init))))),Eof[E])
      )

    iterateeT( F.point( Cont(step(init)) ) )
  }

  /**
   * Folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   * If there is a ping pong on enumerator -> Cont -> enumerator then we'll of course get an infinite loop.
   *
   * foldI returns a ResumableIter that performs a fold until a done, this folds over the enumerator to return a value.
   *
   * combine with onDone to get through chunks of data.
   */
  def foldOnDone[E, F[_], A, ACC]( e: => EnumeratorT[E, F] )( initAcc : ACC, initResumable : ResumableIter[E, F,A] )( f : (ACC, A) => ACC )(implicit F: Monad[F]) : F[ACC] = {
    import ScalesUtils._
    import scalaz.Scalaz._

    val starter = (initResumable &= e).eval

    val r =
      (foldIM[ACC,F,(ACC, ResumableIter[E,F,A], Boolean)]((p, a) => {
        val (currentA, itr, _) = a
        for {
          step <- itr.value
          isdone = isDoneS(step)
          iseof = isEOFS(step)

          shouldStop = (currentA, itr, true)

          res =
            if (isdone && !iseof) {
              val a = extractS(step)
              if (a.isEmpty)
                shouldStop
              else
                (f(currentA, a.get), extractContS(step), false)
            } else
              shouldStop

        } yield {
          val (currentA, itr, done) = res

          (currentA, (itr &= e).eval, done)
        }
      })(init = (initAcc, starter, false), stopOn = a => a._3) &= repeat[ACC,F](initAcc) ) run

    F.map( r ) { r =>
      val ((acc, nr, b), cont) = r
      acc
    }
  }

  /**
   * Only possible to work on IO as it allows us to exit the monad
   * @param e
   * @param init
   * @tparam E
   * @tparam A
   */
  class ResumableIterIterator[E, A]( e : EnumeratorT[E, IO])(init : ResumableIter[E, IO,A]) extends Iterator[A] {
    import ScalesUtils._

    var cur = (init &= e).eval

    def next = {
      val t =
        (for {
          step <- cur.value
        } yield
          extractS(step)
        ).unsafePerformIO

      cur = (extractCont(cur) &= e).eval
      t.get // note we check for defined in hasNext
    }

    def hasNext =
      (for {
        step <- cur.value
        isdone = isDoneS(step)
        r = extractS(step)
      } yield {
        isdone && !isEOFS(step) && r.isDefined
      }).unsafePerformIO
  }

  /**
   * Converts the iteratee/enumerator/source triple into a Iterator.  This is only possible in IO given the ability to exit the monad _and_ trampoline.
   *
   * For most uses iterate is a better option
   */
  def withIter[E, A]( e : EnumeratorT[E, IO])(initResumable : ResumableIter[E, IO,A]) = new ResumableIterIterator[E, A](e)(initResumable)

  /**
   * onDone, iterates over the list of iteratees applying
   * the element, when the resulting list contains a Done
   * then the list of Dones is returned.
   *
   * One can use tuples or pattern matching to discern which
   * of the original lists iteratees have matched.
   *
   * Due to the return type of this iteratee all items
   * in the list must return the same type and must return
   * both an A and an IterV with the same interface to continue
   * with the next and maintain state.
   *
   *
   * In the case of EOF, an empty list is returned
   */
  def onDone[E, F[_],A](originalList : List[ResumableIter[E, F,A]])(implicit F: Monad[F]) : ResumableIterList[E, F,A] = {

    def step(l : List[ResumableIter[E, F,A]])(s: Input[E]): ResumableIterList[E, F,A] = {
      iterateeT(
        s(el = e => {
          @inline def add(res: List[A], newl: List[ResumableIter[E, F, A]], k: (Input[E]) => IterateeT[E, F, (A, IterateeT[E, F, _])]): F[(List[A], List[ResumableIter[E, F, A]])] = {
            val d = k(Element(e))
            F.map(d.value) { step =>
              val nextl = d :: newl
              step(
                done = (x, _) => (x._1 :: res, nextl),
                cont = _ => (res, nextl)
              )
            }
          }

          val r =
            (foldM[ResumableIter[E, F, A], F, (List[A], List[ResumableIter[E, F, A]])]((Nil, Nil)) { (acc, i) =>
              val (res, newl) = acc
              F.bind(i.value){ step => step(
                // safety first
                done = (e1, y) =>
                    // are we EOF, in which case remove
                    if (y.isEof)
                      F.point(acc)
                    else {
                      if (y.isEmpty)
                        // feed back the continuation
                        // this is where we hope that the users don't
                        // break on purpose :-()
                        e1._2.foldT(
                          cont = k => add(res, newl, k.asInstanceOf[(Input[E]) => IterateeT[E, F, (A, IterateeT[E, F, _])]]),
                          done = (x, y) => error("Continuation can only be a Cont")
                        )
                      else
                        error("Can only handle EOF or Empty for Done")

                    }
                ,
                cont = i => add(res, newl, i)
              )}
            } &= iteratorEnumerator(l.iterator)).run

          F.map(r) { r =>
            val (res, newl) = r

            if (res.isEmpty)
              Cont(step(newl))
            else
              Done((res, iterateeT(F.point(Cont(step(newl))))), Empty[E])
          }
        },
        empty = F.point( Cont(step(l)) ),
        eof = F.point( Done((Nil, iterateeT( F.point( Cont(step(l))))), Eof[E]) )
        )
      )
    }

    iterateeT( F.point(  Cont(step(originalList)) ) )
  }

  /**
   * keeps a running count of each element, probably not of much use
   * unless combined but acts as a good poc for ResumableIter
   */
  def runningCount[E, F[_]](implicit F: Monad[F]) = foldI[E, F,Long]((e :E, count : Long) => count + 1 )(0)

  /**
   * Append to an appendable, always returns Done for a line, cont for
   * everything else
   * TODO - should it be a pair including the appendable?
   */
  def appendTo[F[_]]( to : Appendable )(implicit F: Applicative[F]) : IterateeT[CharSequence, F, CharSequence] = {
    def step(s: Input[CharSequence]): IterateeT[CharSequence, F, CharSequence] =
      iterateeT( F.point( s(el = {e =>
          to.append(e)
          Done(e, Empty[CharSequence])},
        empty = Cont(step),
        eof = Done("",Eof[CharSequence])) ) )

    iterateeT( F.point( Cont(step) ) )
  }

  /**
   * Calls the function param with the fed data and returns its
   * result - consider Scalaz 7 map directly rather than composing
   */
  def evalWith[FROM, F[_],TO]( f : (FROM) => TO )(implicit F: Applicative[F]) : IterateeT[FROM, F, TO] = {
    def step(s: Input[FROM]): IterateeT[FROM, F, TO] =
      iterateeT( F.point( s(el = e => {
          val to = f(e)
          Done(to, Empty[FROM])
          },
        empty = Cont(step),
        eof = Cont(step))) )

    iterateeT( F.point( Cont(step) ) )
  }

  /**
   * Enumeratee that converts input 1:1
   * String => Int, enumerator Iterator[String] but IterV[Int, Int]
   */
  @deprecated(since="0.6.0-M5", message="Use Scalaz 7 IterateeT.contramap")
  def enumerateeMap[E, F[_], A, R](target: IterateeT[A,F,R])(f : E => A )(implicit F: Monad[F]) : IterateeT[E, F, R] = {

    def next( i : IterateeT[A,F,R] ) : IterateeT[E, F, R] =
      iterateeT( i.foldT[StepT[E, F, R]](
        done = (a, y) => F.point( Done(a, Eof[E]) ),
        cont = k => F.point( Cont((x: Input[E]) =>
          x( el = e => next(k(Element(f(e)))),
            empty = next(k(Empty[A])),
             eof = next(k(Eof[A])))
        )  )
      ) )

    next(target)
  }

  /**
   * Sums an iteratee up, consider using the Scalaz IterateeT monadic sum instead
   */
  def sum[T, F[_]](implicit n: Numeric[T], F: Applicative[F]): IterateeT[T, F, T] = {
    import n._
    def step(acc: T)( s : Input[T] ) : IterateeT[T, F, T] =
      s( el = e =>
          cont(step(acc + e)),
        empty =
          cont(step(acc)),
        eof =
          done(acc, Eof[T])
      )

    cont(step(zero))
  }

  /**
   * Maps a given input to a function returning a Input[EphemeralStream].
   * If the Input is El it returns it, if EOF empty and continues on empty.
   */
  def mapTo[E, F[_], A]( f: E => Input[EphemeralStream[A]] )(implicit F: Applicative[F]): IterateeT[E, F, EphemeralStream[A]] = {
    def step(s: Input[E]): IterateeT[E, F, EphemeralStream[A]] =
      iterateeT( F.point(
        s(
          el = e => {
            val r = f(e)
            r(
              el = e1 => {
                Done(e1, Empty[E])
              },
              empty = Cont(step),
              eof = Done(EphemeralStream.emptyEphemeralStream, Eof[E])
            )
          },
          empty = Cont(step),
          eof = Done(EphemeralStream.emptyEphemeralStream, Eof[E])
        )
      ) )

    iterateeT( F.point( Cont(step)  ))
  }

  /**
   * Enumeratee that folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   *
   * Converts ResumableIters on Done via a fold, returning Done only when receiving EOF from the initIter.
   *
   * NB - This can be thought of the reverse of toResumableIter but also accumulating.
   */
  def foldOnDoneIter[E, F[_], A, ACC]( initAcc : ACC, initIter : ResumableIter[E,F, A])( f : (ACC, A) => ACC )(implicit F: Monad[F]) : IterateeT[E, F, ACC] = {

    def next( acc : ACC, i : ResumableIter[E,F,A]) : IterateeT[E, F, ACC] =
      iterateeT(
        i.foldT[StepT[E, F, ACC]](
          done = (ac, y) => {
            val (e, cont) = ac
            val newacc = f(acc, e)
            y match {
              case Element(a) =>
                error("Cannot process an input element from Done")
              case Empty() =>
                // can continue
                F.point( Cont[E, F, ACC]( (x : Input[E]) =>
                  // is the cont itself actually a done or a cont?
                  next(newacc,
                    iterateeT(
                      F.bind(cont.value) {
                        s =>
                          s(done = (x, y) => error("got a Done from a resumableIter cont " + x + " " + y),
                          cont = k =>
                            k(x).asInstanceOf[ResumableIter[E, F, A]].value
                        )
                      }
                    )
                  )))
              case Eof() => F.point( Done(newacc, Eof[E]) )
            }
          },
          cont = k => F.point( Cont((x: Input[E]) =>
            next(acc, k(x))) )
        )
      )

    next(initAcc, initIter)
  }

  /**
   * Takes Input[E] converts via toMany to an EphemeralStream[A].  This in turn is fed to the destination iteratee.
   * The inputs can be 1 -> Many, Many -> 1, or indeed 1 -> 1.
   * 
   * The callers must take care of what kind of continnuation Iteratee is returned in a Done.
   *
   * If the dest returns EOF, the toMany is in turn called with EOF for any needed resource control processing.
   */ 
  def enumToMany[E, F[_], A, R]( dest: ResumableIter[A,F,R])( toMany: ResumableIter[E, F, EphemeralStream[A]])(implicit F: Monad[F]): ResumableIter[E, F, R] = {
    val empty = () => EphemeralStream.emptyEphemeralStream[A]

    /**
     * Pumps data from the toMany through to the destination, when the destination has consumed as much as possible it returns.
     */
    def loop( i: ResumableIter[A,F,R], s: () => EphemeralStream[A] ):
      (ResumableIter[A,F,R], () => EphemeralStream[A]) = {

      var cs: EphemeralStream[A] = s() // need it now

      def step(theStep: F[ResumableStep[A, F, R]]): F[ResumableStep[A, F, R]] =
        F.bind(theStep){
            cstep =>
              if (!isDoneS(cstep) && !cs.isEmpty) {
                // println("doing a loop "+c)
                val nc = cstep(
                  done = (a, y) => error("Should not get here"), // send it back, shouldn't be possible to get here anyway due to while test
                  cont =
                    k => {
                      // println("got cont")
                      val cs_called = cs
                      val head = cs_called.headOption.get // if used in El it captures the byname not the value
                      cs = cs_called.tailOption.get
                      k(Element(head))
                    }
                )
                step(nc.value)
              } else
                theStep
          }

      (iterateeT(step(i.value)), () => cs)
    }

    /**
     * Call the toMany continuation function
     */
    def pumpNext(x: Input[E], toMany: Input[E] => ResumableIter[E,F, EphemeralStream[A]], k: Input[A] => ResumableIter[A,F,R] ): ResumableIter[E, F, R] = {

      /*
       println("and then I was here "+
         x(el = e => e.toString,
         empty = "I'm empty ",
         eof = "I'm eof"))
       */
      val afterNewCall = toMany(x)
      //println("and then " + afterNewCall)
      import scalaz._
      import Scalaz._
      iterateeT(
        afterNewCall.foldT(
          done = (nextContPair, rest) => {
            //println("was done weren't it")
            val (e1, nextCont) = nextContPair
            val nextContR = nextCont.asInstanceOf[ResumableIter[E,F,scalaz.EphemeralStream[A]]]
            val r = F.bind(isEOF(afterNewCall)){ eof =>
              (
                if (eof)
                  //println("after is eof")
                  next(k(Eof[A]), empty, nextContR)
                else {
                  if (e1.isEmpty)
                    //println("empty on nextcontr")
                    next(k(Empty[A]), empty, nextContR)
                  else {
                    val h = e1.headOption.get
                    //println("some data after all "+h)
                    val tail = e1.tailOption.get
                    next(k(Element(h)), () => tail, nextContR)
                  }
                }
              ).value
            }

            r
          },
          cont = k1 => {
            //println("conted after here")
            next(k(Empty[A]), empty, afterNewCall).value
          }
        )
      )
    }

    /**
     * For Cont handling we must loop when there is more data left on the stream,
     * when not verify if the toMany has returned more data to process.
     */
    def contk( k: Input[A] => ResumableIter[A,F,R],  i: ResumableIter[A,F,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, F,EphemeralStream[A]] ): ResumableIter[E, F, R] = {
      if (!s().isEmpty) {
        val (ni, ns) = loop(i, s)
        //println("empty against the s "+ni + " " + ns().isEmpty)
        //if (isDone(ni))
        next(ni, ns, toMany) // bad - should let a done exit early
      } else
        iterateeT( F.point( Cont((x: Input[E]) =>
          x(
            el = e => {
              //println("got a cont x el e "+e)
              iterateeT(
                toMany.foldT (
                  done = (a, y) => {
                    val (e1, nextContR) = a
                    val nextCont = nextContR.asInstanceOf[ResumableIter[E,F,scalaz.EphemeralStream[A]]]
                    error("Unexpected State for enumToMany - Cont but toMany is done")
                  },
                  cont = toManyCont => {
                    pumpNext(x, toManyCont, k).value
                  }
                )
              )
            },
            empty = next(k(Empty[A]), empty, toMany),
            eof = next(k(Eof[A]), empty, toMany)
          )
        )))
    }

    /**
     * Handle closed states in either dest or toMany, feed data back out when
     * dest signals it is Done.  .run triggers EOFs but in the case
     * of continuations the data is fake - triggered by doneWith itself.
     * internalEOF caters for this case.
     */
    def doneWith(a: (R, ResumableIter[A,F,R]), y: Input[A], i: ResumableIter[A,F,R], s: () => EphemeralStream[A], toMany: ResumableIter[E,F, EphemeralStream[A]], internalEOF: Boolean ): ResumableIter[E, F, R] = {

      val (res, nextCont) = a
      // println("res is "+ res)
      import scalaz._
      import Scalaz._

      val returnThis : ResumableIter[E, F,R] =
        iterateeT(
          for{
            eof <- isEOF(nextCont)
            doneMany <- isDone(toMany)
            eofMany <- isEOF(toMany)
          } yield {
            if (eof ||
                (doneMany && eofMany)     || // either eof then its not restartable
                (Eof.unapply(y) && !internalEOF )  // or the source is out of elements
              )

              resumableEOFDone[E,F,R](res)

            else
              // there is a value to pass back out
              Done((res, {
                def ocont = next(nextCont, s, toMany, true)

                if (s().isEmpty) {
                  // need to process this res but force another to be
                  // calculated before it is returned to the enumerator
                  //cont[E,F,(R,IterateeT[E,F,_])]( _ => cont[E,F,(R,IterateeT[E,F,_])]( _ => ocont))
                  cont[E,F,(R,IterateeT[E,F,_])]( _ => ocont)
                } else {
                  // still data to process
                  ocont
                }
              }), Empty[E])
          }
        )

      if (Eof.unapply(y) && !internalEOF) {
        // signal the end here to toMany, don't care about result, tested by testSimpleLoad and testRandomAmounts in AsyncPullTest
        iterateeT( F.map(returnThis.value){ v =>
          toMany.foldT(done= (a1, y1) => F.point(false),
                cont = k => {
                  k(Eof[E]); F.point(false)
                })
          v
        })
      } else
        returnThis
    }

    def next( i: ResumableIter[A,F,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, F,EphemeralStream[A]], internalEOF: Boolean = false ): ResumableIter[E, F,R] =
      iterateeT(
          F.bind(i.value)(step => step.fold(
            cont = k => contk(k, i, s, toMany).value
            , done = (a, y) =>
              doneWith(a.asInstanceOf[(R, ResumableIter[A, F, R])], y, i, s, toMany, internalEOF).value
          )))

    next(dest, empty, toMany)
  }


  /**
   * Based on Scalaz 7 flatMap but exposes the monad through the f parameter
   * @param itr
   * @param f
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @tparam B
   * @return
   */
  def flatMap[E,F[_],A,B](itr: IterateeT[E,F,A])(f: A => IterateeT[E, F, B])(implicit F: Monad[F]): IterateeT[E, F, B] = {
    def through(x: IterateeT[E, F, A]): IterateeT[E, F, B] =
      iterateeT(
        F.bind(x.value)((s: StepT[E, F, A]) => s.fold[F[StepT[E, F, B]]](
          cont = k => F.point(StepT.scont(u => through(k(u))))
          , done = (a, i) =>
            if (i.isEmpty)
              f(a).value
            else
              F.bind(f(a).value)(_.fold(
                cont = kk => kk(i).value
                , done = (aa, _) => F.point(StepT.sdone[E, F, B](aa, i))
              ))
        )))

    through(itr)
  }

  def iterateesOf[F[_]](implicit F: Monad[F]) = new IterateeFunctions[F](F)

  val ioIteratees = iterateesOf[IO]
  val trampolineIteratees = iterateesOf[Trampoline]
  /**
   * Warning: Id does not trampoline, consider trampolineIteratees or ioIteratees to import from
   */
  val idIteratees = iterateesOf[Id]

}

class IterateeFunctions[F[_]](val F: Monad[F]) {

  type TheF[X] = F[X]

  object EphemeralStreamEnum {

    /**
     * Enumerates over an EphemeralStream
     * @param xs
     * @tparam E
     * @tparam F
     * @return
     */
    @inline def enumEphemeralStream[E](xs: EphemeralStream[E])(implicit F: Monad[F]): EnumeratorT[E, F] =
      scales.utils.iteratee.EphemeralStreamEnum.enumEphemeralStream[E,F](xs)

    /**
     * Enumerates over an EphemeralStream but allows progress to be "saved" between calls
     * @param state
     * @param xs
     * @tparam E
     * @tparam F
     * @return
     */
    @inline def enumEphemeralStreamF[E](state: EphemeralStream[E] => Unit)(xs: EphemeralStream[E])(implicit F: Monad[F]): EnumeratorT[E, F] =
      scales.utils.iteratee.EphemeralStreamEnum.enumEphemeralStreamF[E, F](state)(xs)

    /**
     * Converts an iterator to EphemeralStream.  EphemeralStream then ensures next will not be called unexpectedly in the face of trampolining
     * @param iterator
     * @tparam A
     * @return
     */
    @inline def toEphemeral[A](iterator: Iterator[A]): EphemeralStream[A] =
      scales.utils.iteratee.EphemeralStreamEnum.toEphemeral[A](iterator)
  }

  object IterateeImplicits {
    @inline implicit def toEval[WHAT, RETURN]( i : IterateeT[WHAT, F, RETURN] ) =
      scales.utils.ScalesUtils.toEval[WHAT,F,RETURN](i)

    /**
     * Taken from huynhjl's answer on StackOverflow, just abstracting the type to allow for better implicit inference
     *
     * def iteratorEnumerator[E](iter: Iterator[E]) = new EnumeratorT[E, Trampoline] {
     * override def apply[A]: StepT[E, Trampoline, A] => IterateeT[E, Trampoline, A] =
     * {
     * case step if iter.isEmpty => iterateeT[E, Trampoline, A](Free.point(step))
     * case step @ Done(acc, input) => iterateeT[E, Trampoline, A](Free.point(step))
     * case step @ Cont(k) =>
     * val x : E = iter.next
     *
     * k(Element(x)) >>== {
     * s => s.mapContOr(_ => sys.error("diverging iteratee"), apply(s))
     * }
     * }
     * }
     */
  }

  @inline def iteratorEnumerator[E](iter: Iterator[E])(implicit F: Monad[F]): EnumeratorT[E, F] =
    scales.utils.iteratorEnumerator[E,F](iter)
  @inline def error(string: String) = scales.utils.error(string)

  /** drop while iteratee, returning the possibly remaining data */
  @inline def dropWhile[E](f: (E) => Boolean)(implicit F: Monad[F]): IterateeT[E, F, Option[E]] =
    scales.utils.dropWhile[E,F](f)

  /**
   * Same as dropWhile but captures the Monad in F
   */
  @inline def dropWhileM[E](f: (E) => F[Boolean])(implicit F: Monad[F]): IterateeT[E, F, Option[E]] =
    scales.utils.dropWhileM[E,F](f)

  /** "find" iteratee, finds Some(first) or None */
  @inline def find[E](f: (E) => Boolean)(implicit F: Monad[F]): IterateeT[E, F, Option[E]] =
    scales.utils.find[E,F](f)

  /** filter iteratee, greedily taking all content until eof */
  @inline def filter[E](f: (E) => Boolean)(implicit F: Monad[F]): IterateeT[E, F, Iterable[E]] =
    scales.utils.filter[E,F](f)

  // defined again to make sure imports are smooth

  type ResumableIterList[E, A] = IterateeT[E, F, (Iterable[A],IterateeT[E,F,_])]
  type ResumableIterListStep[E, A] = StepT[E, F, (Iterable[A],IterateeT[E,F,_])]
  type ResumableIter[E, A] = IterateeT[E, F, (A, IterateeT[E, F,_])]
  type ResumableStep[E, A] = StepT[E, F, (A, IterateeT[E, F, _])]


  /**
   * marks a continuation resumableiter as actually being EOF - i.e. don't attempt to evaluate it
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @return
   */
  @inline def resumableEOF[E, A](input: A = null)(implicit F: Monad[F]): ResumableIter[E, A] =
    scales.utils.resumableEOF[E,F,A](input)

  @inline def resumableEOFDone[E, A](input: A)(implicit F: Monad[F]): ResumableStep[E, A] =
    scales.utils.resumableEOFDone[E,F,A](input)

  /**
   * is this iteratee actually "empty"
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @return
   */
  @inline def isResumableEOF[E, A](iter: ResumableIter[E,A])(implicit F: Monad[F]): F[Boolean] =
    scales.utils.isResumableEOF[E,F,A](iter)

  @inline def isResumableEOFS[E, A](s: ResumableStep[E,A])(implicit F: Monad[F]): Boolean =
    scales.utils.isResumableEOFS[E,F,A](s)

  /**
   * Extract the continuation from a Done
   */
  @inline def extractCont[E, A]( iter : ResumableIter[E,A] )(implicit F: Monad[F]): ResumableIter[E, A] =
    scales.utils.extractCont[E,F,A](iter)

  /**
   * Extract the continuation from a Done
   */
  @inline def extractContS[E, A]( s : ResumableStep[E, A] )(implicit F: Monad[F]): ResumableIter[E, A] =
    scales.utils.extractContS[E,F,A](s)

  /**
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  @inline def extract[E, A]( iter : ResumableIter[E,  A] )(implicit F: Monad[F]) : F[Option[A]] =
    scales.utils.extract[E,F,A](iter)

  /**
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  @inline def extractS[E, A]( iter : ResumableStep[E, A] )(implicit F: Monad[F]) : Option[A] =
    scales.utils.extractS[E,F,A](iter)

  /**
   * Helper to identify dones
   */
  @inline def isDone[E, A]( iter : IterateeT[E, F, A] )(implicit F: Monad[F]): F[Boolean] =
    scales.utils.isDone[E,F,A](iter)

  @inline def isDoneS[E, A]( step : StepT[E, F, A] )(implicit F: Monad[F]): Boolean =
    scales.utils.isDoneS[E,F,A](step)

  /**
   * Helper for done and empty
   */
  @inline def isEmpty[E, A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    scales.utils.isEmpty(iter)

  /**
   * Helper for done and eof
   */
  @inline def isEOF[E, A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    scales.utils.isEOF[E,F,A](iter)

  /**
   * Helper for done and empty
   */
  @inline def isEmptyS[E,A]( step : StepT[E,F,A] )(implicit F: Monad[F]): Boolean =
    scales.utils.isEmptyS[E,F,A](step)

  /**
   * Helper for done and eof
   */
  @inline def isEOFS[E, A]( step : StepT[E,F,A] )(implicit F: Monad[F]): Boolean =
    scales.utils.isEOFS[E,F,A](step)

  /**
   * Converts a normal IterV[E,A] to a ResumableIter.
   *
   * Does so by folding over the iter once for an input
   * and when its Done starting again
   * with the original iter.  This is close to restarting the iter on
   * a new "stream", otherwise all attempts to keep the Cont will be made.
   */
  @inline implicit def toResumableIter[E,A]( oiter : IterateeT[E,F,A]) (implicit F: Monad[F]): ResumableIter[E,A] =
    scales.utils.toResumableIter[E,F,A](oiter)

  /**
   * Stepwise fold, each element is evaluated but each one is returned as a result+resumable iter.
   */
  @inline def foldI[E,A]( f : (E,A) => A )( init : A, stopOn: A => Boolean = (_: A) => true )(implicit F: Monad[F]) : ResumableIter[E,A] =
    scales.utils.foldI[E,F,A](f)(init, stopOn)

  /**
   * Stepwise fold but the result of f is bound to F
   */
  @inline def foldIM[E,A]( f : (E,A) => F[A] )( init : A, stopOn: A => Boolean = (_: A) => true )(implicit F: Monad[F]) : ResumableIter[E,A] =
    scales.utils.foldIM[E,F,A](f)(init, stopOn)

  /**
   * Folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   * If there is a ping pong on enumerator -> Cont -> enumerator then we'll of course get an infinite loop.
   *
   * foldI returns a ResumableIter that performs a fold until a done, this folds over the enumerator to return a value.
   *
   * combine with onDone to get through chunks of data.
   */
  @inline def foldOnDone[E, A, ACC]( e: => EnumeratorT[E, F] )( initAcc : ACC, initResumable : ResumableIter[E, A] )( f : (ACC, A) => ACC ) (implicit F: Monad[F]): F[ACC] =
    scales.utils.foldOnDone[E,F,A,ACC](e)(initAcc, initResumable)(f)

  /**
   * onDone, iterates over the list of iteratees applying
   * the element, when the resulting list contains a Done
   * then the list of Dones is returned.
   *
   * One can use tuples or pattern matching to discern which
   * of the original lists iteratees have matched.
   *
   * Due to the return type of this iteratee all items
   * in the list must return the same type and must return
   * both an A and an IterV with the same interface to continue
   * with the next and maintain state.
   *
   *
   * In the case of EOF, an empty list is returned
   */
  @inline def onDone[E, A](originalList : List[ResumableIter[E, A]]) (implicit F: Monad[F]): ResumableIterList[E, A] =
    scales.utils.onDone[E,F,A](originalList)

  /**
   * keeps a running count of each element, probably not of much use
   * unless combined but acts as a good poc for ResumableIter
   */
  @inline def runningCount[E](implicit F: Monad[F]) =
    scales.utils.runningCount[E,F]

  /**
   * Append to an appendable, always returns Done for a line, cont for
   * everything else
   * TODO - should it be a pair including the appendable?
   */
  @inline def appendTo( to : Appendable )(implicit F: Applicative[F]): IterateeT[CharSequence, F, CharSequence] =
    scales.utils.appendTo[F](to)

  /**
   * Calls the function param with the fed data and returns its
   * result - consider Scalaz 7 map directly rather than composing
   */
  @inline def evalWith[FROM, TO]( f : (FROM) => TO )(implicit F: Applicative[F]): IterateeT[FROM, F, TO] =
    scales.utils.evalWith[FROM,F,TO](f)

  /**
   * Enumeratee that converts input 1:1
   * String => Int, enumerator Iterator[String] but IterV[Int, Int]
   */
  @deprecated(since="0.6.0-M5", message="Use Scalaz 7 IterateeT.contramap")
  @inline def enumerateeMap[E, A, R](target: IterateeT[A,F,R])(f : E => A )(implicit F: Monad[F]) : IterateeT[E, F, R] =
    scales.utils.enumerateeMap[E,F,A,R](target)(f)

  /**
   * Sums an iteratee up, consider using the Scalaz IterateeT monadic sum instead
   */
  @inline def sum[T](implicit n: Numeric[T], F: Applicative[F]): IterateeT[T, F, T] =
    scales.utils.sum[T,F]

  /**
   * Maps a given input to a function returning a Input[EphemeralStream].
   * If the Input is El it returns it, if EOF empty and continues on empty.
   */
  @inline def mapTo[E, A]( f: E => Input[EphemeralStream[A]] )(implicit F: Applicative[F]): IterateeT[E, F, EphemeralStream[A]] =
    scales.utils.mapTo[E,F,A](f)

  /**
   * Enumeratee that folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   *
   * Converts ResumableIters on Done via a fold, returning Done only when receiving EOF from the initIter.
   *
   * NB - This can be thought of the reverse of toResumableIter but also accumulating.
   */
  @inline def foldOnDoneIter[E,A, ACC]( initAcc : ACC, initIter : ResumableIter[E, A])( f : (ACC, A) => ACC )(implicit F: Monad[F]): IterateeT[E, F, ACC] =
    scales.utils.foldOnDoneIter[E,F,A,ACC](initAcc, initIter)(f)

  /**
   * Takes Input[E] converts via toMany to an EphemeralStream[A].  This in turn is fed to the destination iteratee.
   * The inputs can be 1 -> Many, Many -> 1, or indeed 1 -> 1.
   *
   * The callers must take care of what kind of continnuation Iteratee is returned in a Done.
   *
   * If the dest returns EOF, the toMany is in turn called with EOF for any needed resource control processing.
   */
  @inline def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]])(implicit F: Monad[F]): ResumableIter[E, R] =
    scales.utils.enumToMany[E,F,A,R](dest)(toMany)


  /**
   * Based on Scalaz 7 flatMap but exposes the monad through the f parameter
   * @param itr
   * @param f
   * @param F
   * @tparam E
   * @tparam A
   * @tparam B
   * @return
   */
  @inline def flatMap[E,A,B](itr: IterateeT[E,F,A])(f: A => IterateeT[E, F, B])(implicit F: Monad[F]): IterateeT[E, F, B] =
    scales.utils.flatMap[E,F,A,B](itr)(f)

}