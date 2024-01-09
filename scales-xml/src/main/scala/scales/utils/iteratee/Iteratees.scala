package scales.utils.iteratee

import scalaz.{Applicative, EphemeralStream, Monad}
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.Iteratee.{cont, done, elInput, empty, enumEofT, foldM, iterateeT}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{EnumeratorT, Input, IterateeT, StepT}
import scales.utils.ScalesUtils
import scales.utils.ScalesUtils.{iteratorEnumerator, toEval}

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
}

/**
 * Collection of iterateees
 */
trait Iteratees {

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
    F.map(iter.value)(isResumableEOF[E,F,A])

  def isResumableEOF[E, F[_], A](s: ResumableStep[E,F,A]): Boolean =
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
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  def extract[E, F[_], A]( iter : ResumableIter[E, F, A] )(implicit F: Monad[F]) : F[Option[A]] =
    iter.foldT[Option[A]](
      done = (x, i) =>
	      F.point(Some(x._1)),
      cont = f => F.point(None) )

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
  def foldI[E,F[_],A]( f : (E,A) => A )( init : A )(implicit F: Monad[F]) : ResumableIter[E,F,A] =
    foldIM[E,F,A]{ (e,a) => F.point(f(e,a)) }(init)

  /**
   * Stepwise fold but the result of f is bound to F
   */
  def foldIM[E,F[_],A]( f : (E,A) => F[A] )( init : A )(implicit F: Monad[F]) : ResumableIter[E,F,A] = {
    def step( current : A )( s : Input[E] ) : IterateeT[E,F,(A, IterateeT[E,F,_])] =
      s(
        el = e =>
          IterateeT.IterateeTMonadTrans[E].liftM(f(e, current)) flatMap{ i =>
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
  def foldOnDone[E, A, F[_], ACC]( e: EnumeratorT[E, F] )( initAcc : ACC, initResumable : ResumableIter[E, F,A] )( f : (ACC, A) => ACC )(implicit F: Monad[F]) : F[ACC] = {
    /*import ScalesUtils._

    var currentI = (initResumable &= e).eval
    var isdone = isDone(currentI)
    var currentA = initAcc
    while( !isdone || (isdone && !isEOF(currentI)) ) {

      if (isdone) {
        val a = extract(currentI)
        if (!a.isDefined)
          return currentA
        else {
          currentA = f(currentA, a.get)
          currentI = extractCont(currentI)
        }
      }

      currentI = (currentI &= e).eval
      isdone = isDone(currentI)
    }
    currentA*/
    ???
  }

  class ResumableIterIterator[E, F[_],A]( e : EnumeratorT[E, F])(init : ResumableIter[E, F,A]) extends Iterator[A] {
    import ScalesUtils._

    /*var cur = (init &= e).eval
    var isdone = isDone(cur)
    var r = extract(cur)
*/
    def next = {
  /*    val t = r
      cur = (extractCont(cur) &= e).eval
      isdone = isDone(cur)
      r = extract(cur)
      t.get // note we check for defined in hasNext*/
      ???
    }

    def hasNext = true//isdone && !isEOF(cur) && r.isDefined
  }

  /**
   * Converts the iteratee/enumerator/source triple into a Iterator, not possible if it's not Id

  def withIter[E, F[_],A]( e : EnumeratorT[E, F])(initResumable : ResumableIter[E, F,A]) = new ResumableIterIterator[E, F,A](e)(initResumable)
   */

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
  def enumerateeMap[E, A, F[_], R](target: IterateeT[A,F,R])(f : E => A )(implicit F: Monad[F]) : IterateeT[E, F, R] = {

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
  def foldOnDoneIter[E,A, F[_], ACC]( initAcc : ACC, initIter : ResumableIter[E,F, A])( f : (ACC, A) => ACC )(implicit F: Monad[F]) : IterateeT[E, F, ACC] = {

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
  def enumToMany[E, A, F[_], R]( dest: ResumableIter[A,F,R])( toMany: ResumableIter[E, F, EphemeralStream[A]])(implicit F: Monad[F], AF: Applicative[F]): ResumableIter[E, F, R] = {
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

  /**
   * Based on the Scalaz 7 map but exposes the monad through the f parameter
   * @param itr
   * @param f
   * @param F
   * @tparam E
   * @tparam F
   * @tparam A
   * @tparam B
   * @return
   */
  def mapStep[E,F[_],A,B](itr: IterateeT[E,F,A])(f: A => F[StepT[E,F,B]])(implicit F: Monad[F]): IterateeT[E, F, B] =
    flatMap(itr)(a => iterateeT(f(a)))

  /*
  /*
        def resumableMap[E, F[_], A, AA](source: ResumableIter[E,F,A])(output: A => AA)(implicit F: Monad[F]): ResumableIter[E, F, AA] =
          (
            mapStep[E,F,A,(AA, ResumableIter[E, F, AA])](source.asInstanceOf[IterateeT[E,F,A]]) { case ((a: A, cont: ResumableIter[E,F,A])) =>
              F.bind(cont.value) {
                contstep => // don't actually care about the value, just need F
                  F.map(isResumableEOF(cont)) {
                    isEOF =>
                    if (isEOF)
                      // don't further evaluate
                      Done[E, F, (AA, ResumableIter[E, F, AA])]((output(a), resumableEOF), eofInput)
                    else
                      Done[E, F, (AA, ResumableIter[E, F, AA])]((output(a), resumableMap[E, F, A, AA](cont)(output)), emptyInput)
                  }
              }
            }
          ).asInstanceOf[ResumableIter[E, F, AA]]

        def resumableContramap[E, A, F[_], R](target: ResumableIter[A,F,R])(f : E => A )(implicit F: Monad[F]) : ResumableIter[E, F, R] = {

          def next( i : ResumableIter[A,F,R] ) : ResumableIter[E, F, R] = {
            scalaz.iteratee.Iteratee.iterateeT( i.foldT[ResumableStep[E, F, R]](
              done = (i, y) => {
                val (a, cont) = i

                F.point( Done[E, F, (R, ResumableIter[E,F, R])](
                  (a,
                    if (!y.isEof)
                      resumableContramap(cont.asInstanceOf[ResumableIter[A,F,R]])(f)
                    else
                      resumableEOF
                  ),
                  if (y.isEof) eofInput else emptyInput).asInstanceOf[ResumableStep[E,F,R]] )
              },
              cont = k => F.point( Cont((x: Input[E]) =>
                x(
                  el = e => next(k(Element(f(e)))),
                  empty = next(k(Empty[A])),
                  eof = next(k(Eof[A])))
              ))
            ) )
          }

          next(target)
        }
    */

   */
}
