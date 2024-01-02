package scales.utils.iteratee

import scalaz.Free.Trampoline
import scalaz.Id.Id
import scalaz.{Applicative, Bind, EphemeralStream, Free, Id, Monad, Trampoline}
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.Iteratee.{cont, done, iteratee, iterateeT}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{Enumerator, EnumeratorT, Input, Iteratee, IterateeT, StepT}
import scales.utils.ScalesUtils
import scales.utils.io

import javax.xml.ws.WebFault

/**
 * Evals once, the developer must check its Done, equivalent to a .run but
 * doesn't lose the continuation - no "Diverging Iteratee"
 */
trait Eval[WHAT, F[_],RETURN] {

  val orig : IterateeT[WHAT, F, RETURN]

  def eval(implicit f: Monad[F]) : F[IterateeT[WHAT, F, RETURN]] =
    orig.foldT(
      done = (x, y) => f.point(iterateeT[WHAT, F, RETURN](f.point(Done(x,y)))),
	    cont = k => f.point(k(Eof[WHAT]))
    )

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
   * } */

  //  def enumIterator[E, F[_]](x: => Iterator[E])(implicit F: MonadIO[F]) : EnumeratorT[E, F] =
  def iteratorEnumerator[E, F[_]](iter: => Iterator[E])(implicit f: Monad[F]): EnumeratorT[E, F] =
    new EnumeratorT[E, F] {
      def apply[A] = {
        def go(xs: Iterator[E])(s: StepT[E, F, A]): IterateeT[E, F, A] =
          if (xs.isEmpty) s.pointI
          else {
            s mapCont { k =>
              val next = xs.next()
              k(Element(next)) >>== go(xs)
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

  /** drop while iteratee */
  @deprecated("Now provided by Scalaz",since = "0.6.0")
  def dropWhile[E, F[_] : Applicative](f: (E) => Boolean) : IterateeT[E, F, Unit] = Iteratee.dropWhile(f)

  /** "find" iteratee, finds Some(first) or None */
  @deprecated("Now provided by Scalaz dropUntil",since = "0.6.0")
  def find[E, F[_]: Applicative](f: (E) => Boolean) : IterateeT[E,F, Unit] = Iteratee.dropUntil(f)

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

  /*
  def RCont[E,A](k: Input[E] => ResumableIter[E,A]): ResumableStep[E,A] = Cont[E, Id, (A, Iteratee[E,_])](k)
  def RDone[E,A](d: => (A, Iteratee[E,_]), r: => Input[E]): ResumableStep[E,A] = Done[E, Id, (A, Iteratee[E,_])](d, r)

  def resumable[E,A](step: ResumableStep[E, A]): ResumableIter[E,A] =
    iteratee(step) */

  /**
   * Extract the continuation from a Done
   */
  def extractCont[E, F[_], A]( iter : ResumableIter[E, F, A] )(implicit F: Monad[F]): F[ResumableIter[E, F, A]] =
    iter.foldT[ResumableIter[E, F, A]](
      done = (x, i) =>
        F.point(x._2.asInstanceOf[ResumableIter[E, F, A]]),
      cont = f => error("Was not done"))

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
    iter.foldT[Boolean](
      done = (a, i) => F.point(true),
      cont = f => F.point(false) )

  /**
   * Helper for done and empty
   */
  def isEmpty[E,F[_],A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    iter.foldT[Boolean](
      done = (a, i) => F.point(Empty.unapply[E](i)),
      cont = f => F.point(error("Iteratee is not Done")) )

  /**
   * Helper for done and eof
   */
  def isEOF[E, F[_], A]( iter : IterateeT[E,F,A] )(implicit F: Monad[F]): F[Boolean] =
    iter.foldT[Boolean](
      done = (a, i) => F.point( Eof.unapply[E](i)),
      cont = f => F.point(error("Iteratee is not Done")) )

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
  def foldI[E,F[_],A]( f : (E,A) => A )( init : A )(implicit F: Monad[F]) : ResumableIter[E,F,A] = {
    def step( current : A )( s : Input[E] ) : ResumableIter[E,F,A] =
      iterateeT(
        s(
          el = {e =>
            val next = f(e,current)
            F.point( Done((next, iterateeT( F.point(Cont(step(next))))), Empty[E]))} ,
          empty = F.point(Cont(step(current))),
          eof = F.point( Done((current, iterateeT( F.point( Cont(step(init))))),Eof[E]) )
          )
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
    import ScalesUtils._

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
    currentA
  }

  class ResumableIterIterator[E,A]( e : Enumerator[E])(init : ResumableIter[E,A]) extends Iterator[A] {
    import ScalesUtils._

    var cur = (init &= e).eval
    var isdone = isDone(cur)
    var r = extract(cur)

    def next = {
      val t = r
      cur = (extractCont(cur) &= e).eval
      isdone = isDone(cur)
      r = extract(cur)
      t.get // note we check for defined in hasNext
    }

    def hasNext = isdone && !isEOF(cur) && r.isDefined
  }

  /**
   * Converts the iteratee/enumerator/source triple into a Iterator
   */
  def withIter[E,A]( e : Enumerator[E])(initResumable : ResumableIter[E,A]) = new ResumableIterIterator[E,A](e)(initResumable)

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
  def onDone[E,A](originalList : List[ResumableIter[E,A]]) : ResumableIterList[E,A] = {

    def step(l : List[ResumableIter[E,A]])(s: Input[E]): ResumableIterList[E,A] = {
      iteratee(
        s(el = e => {
          var res : List[A] = Nil
          var newl : List[ ResumableIter[E,A] ] = Nil

          @inline def add( k : (Input[E]) => Iteratee[E,(A, Iteratee[E, _])]) {
            val d = k(Element(e))
            newl = d :: newl
            d foldT[Unit] (
              done = (x, _) => res = x._1 :: res,
              cont = _ => ()
            )
          }

          val i = l.iterator
          while(i.hasNext) {
            i.next.foldT(
              // safety first
              done = (e1, y) =>
                // are we EOF, in which case remove
                if (isEOF(iteratee(Done(e1,y))))
                  Nil
                else {
                  if (isEmpty(iteratee(Done(e1,y))))
                    // feed back the continuation
                    // this is where we hope that the users don't
                    // break on purpose :-()
                    e1._2.foldT(
                      cont = k => add(k.asInstanceOf[(Input[E]) => Iteratee[E,(A, Iteratee[E, _])]]),
                      done = (x,y) => error("Continuation can only be a Cont")
                    )
                  else
                    error("Can only handle EOF or Empty for Done")

                },
              cont = i => add(i)
            )
          }

          if (res.isEmpty)
            Cont(step(newl))
          else
            Done((res, iteratee(Cont(step(newl)))), Empty[E])

        },
        empty = Cont(step(l)),
        eof = Done((Nil, iteratee(Cont(step(l)))), Eof[E])
        )
      )
    }

    iteratee( Cont(step(originalList)) )
  }

  /**
   * keeps a running count of each element, probably not of much use
   * unless combined but acts as a good poc for ResumableIter
   */
  def runningCount[E] = foldI[E,Long]((e :E, count : Long) => count + 1 )(0)

  /**
   * Append to an appendable, always returns Done for a line, cont for
   * everything else
   * TODO - should it be a pair including the appendable?
   */
  def appendTo( to : Appendable ) : Iteratee[CharSequence, CharSequence] = {
    def step(s: Input[CharSequence]): Iteratee[CharSequence, CharSequence] =
      iteratee( s(el = {e =>
          to.append(e)
          Done(e, Empty[CharSequence])},
        empty = Cont(step),
        eof = Done("",Eof[CharSequence])) )
    iteratee( Cont(step) )
  }

  /**
   * Calls the function param with the fed data and returns its
   * result
   */
  def evalWith[FROM,TO]( f : (FROM) => TO ) : Iteratee[FROM, TO] = {
    def step(s: Input[FROM]): Iteratee[FROM, TO] =
      iteratee( s(el = e => {
          val to = f(e)
          Done(to, Empty[FROM])
          },
        empty = Cont(step),
        eof = Cont(step)) )

    iteratee( Cont(step) )
  }

  /**
   * Enumeratee that converts input 1:1
   * String => Int, enumerator Iterator[String] but IterV[Int, Int]
   */
  def enumerateeMap[E, A, R]( dest : Iteratee[A,R])( f : E => A ) : Iteratee[E, R] = {

    def next( i : Iteratee[A,R] ) : Iteratee[E, R] =
      iteratee( i.foldT[StepT[E, Id, R]](
        done = (a, y) => Done(a, Eof[E]),
        cont = k => Cont((x: Input[E]) =>
          x( el = e => next(k(Element(f(e)))),
            empty = next(k(Empty[A])),
             eof = next(k(Eof[A])))
        )
      ) )

    next(dest)
  }

  /**
   * Sums an iteratee up, consider using the Scalaz IterateeT monadic sum instead
   */
  def sum[T](implicit n: Numeric[T]): Iteratee[T,T] = {
    import n._
    def step(acc: T)( s : Input[T] ) : Iteratee[T, T] =
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
  def mapTo[E, A]( f: E => Input[EphemeralStream[A]] ): Iteratee[E, EphemeralStream[A]] = {
    def step(s: Input[E]): Iteratee[E, EphemeralStream[A]] =
      iteratee(
        s( el = e => {
            val r = f(e)
            r( el = e1 => {
                Done(e1, Empty[E])
              },
              empty = Cont(step),
              eof = Done(EphemeralStream.emptyEphemeralStream, Eof[E])
            )
          },
          empty = Cont(step),
          eof = Done(EphemeralStream.emptyEphemeralStream, Eof[E])
        )
      )

    iteratee( Cont(step) )
  }

    /**
   * Enumeratee that folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   *
   * Converts ResumableIters on Done via a fold, returning Done only when receiving EOF from the initIter.
   *
   * NB - This can be thought of the reverse of toResumableIter but also accumulating.
   */
  def foldOnDoneIter[E,A, ACC]( initAcc : ACC, initIter : ResumableIter[E,A])( f : (ACC, A) => ACC ) : Iteratee[E, ACC] = {

    def next( acc : ACC, i : ResumableIter[E,A]) : Iteratee[E, ACC] =
      iteratee(
        i.foldT[StepT[E, Id, ACC]](
          done = (ac, y) => {
            val (e, cont) = ac
            val newacc = f(acc, e)
            y match {
              case Element(a) =>
                error("Cannot process an input element from Done")
              case Empty() =>
                // can continue
                Cont( (x : Input[E]) =>
                  // is the cont itself actually a done or a cont?
                  next(newacc,
                       cont foldT(
                         done = (x,y) => error("got a Done from a resumableIter cont "+ x +" "+y),
                         cont = k =>
                            k(x).asInstanceOf[ResumableIter[E,A]]
                       )
                  ))
              case Eof() => Done(newacc, Eof[E])
            }
          },
          cont = k => Cont((x: Input[E]) =>
            next(acc, k(x)))
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
  def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R] = {
    val empty = () => EphemeralStream.emptyEphemeralStream[A]

    /**
     * Pumps data from the toMany through to the destination, when the destination has consumed as much as possible it returns.
     */
    def loop( i: ResumableIter[A,R], s: () => EphemeralStream[A] ):
      (ResumableIter[A,R], () => EphemeralStream[A]) = {
      var c: ResumableIter[A,R] = i
      var cs: EphemeralStream[A] = s() // need it now
	    //println("loopy "+ isDone(c)+ " " +cs.isEmpty)
      while(!isDone(c) && !cs.isEmpty) {
        // println("doing a loop "+c)
        val (nc, ncs): (ResumableIter[A,R], EphemeralStream[A]) = c.foldT(
          done = (a, y) => (c, s()),// send it back, shouldn't be possible to get here anyway due to while test
          cont =
            k => {
              // println("got cont")
              val cs_called = cs
              val head = cs_called.headOption.get // if used in El it captures the byname not the value
              (k(Element(head)), cs_called.tailOption.get)
            }
            )
        c = nc
        cs = ncs
      }

      (c, () => cs)
    }

    /**
     * Call the toMany continuation function
     */
    def pumpNext(x: Input[E], toMany: Input[E] => ResumableIter[E,EphemeralStream[A]], k: Input[A] => ResumableIter[A,R] ): ResumableIter[E, R] = {

      /*
       println("and then I was here "+
         x(el = e => e.toString,
         empty = "I'm empty ",
         eof = "I'm eof"))
       */
      val afterNewCall = toMany(x)
      //println("and then " + afterNewCall)

      afterNewCall.foldT(
        done = (nextContPair, rest) => {
          //println("was done wern't it")
          val (e1, nextCont) = nextContPair
          val nextContR = nextCont.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
          if (isEOF(afterNewCall)) {
            //println("after is eof")
            next(k(Eof[A]), empty, nextContR)
          } else {
            if (e1.isEmpty) {
              //println("empty on nextcontr")
              next(k(Empty[A]), empty, nextContR)
            }
            else {
              val h = e1.headOption.get
              // println("some data after all "+h)
              val tail = e1.tailOption.get
              next(k(Element(h)), () => tail, nextContR)
            }
          }
        },
        cont = k1 => {
          //println("conted after here")
          next(k(Empty[A]), empty, afterNewCall)
        }
      )
    }

    /**
     * For Cont handling we must loop when there is more data left on the stream,
     * when not verify if the toMany has returned more data to process.
     */
    def contk( k: Input[A] => ResumableIter[A,R],  i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, R] = {
      if (!s().isEmpty) {
        val (ni, ns) = loop(i, s)
        //println("empty against the s "+ni + " " + ns().isEmpty)
        //if (isDone(ni))
        next(ni, ns, toMany) // bad - should let a done exit early
      } else
        iteratee( Cont((x: Input[E]) =>
          x( el = e => {
            //println("got a cont x el e "+e)
            toMany.foldT (
              done = (a, y) => {
                val (e1, nextContR) = a
                val nextCont = nextContR.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
                error("Unexpected State for enumToMany - Cont but toMany is done")
              },
              cont = toManyCont => {
                pumpNext(x, toManyCont, k)
              }
            )
          },
            empty = {
              next(k(Empty[A]), empty, toMany)
            },
            eof = {
              next(k(Eof[A]), empty, toMany)
            }
          )
        ) )
    }

    /**
     * Handle closed states in either dest or toMany, feed data back out when
     * dest signals it is Done.  .run triggers EOFs but in the case
     * of continuations the data is fake - triggered by doneWith itself.
     * internalEOF caters for this case.
     */
    def doneWith(a: (R, ResumableIter[A,R]), y: Input[A], i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]], internalEOF: Boolean ): ResumableIter[E, R] = {

      val (res, nextCont) = a
      // println("res is "+ res)

      val returnThis : ResumableIter[E, R] =
        iteratee(
          if ((isDone(nextCont) && isEOF(nextCont)) ||
              (isDone(toMany) && isEOF(toMany))     || // either eof then its not restartable
              (Eof.unapply(y) && !internalEOF )  // or the source is out of elements
            ) {

            Done((res, iteratee(Done(res, Eof[E]))), Eof[E])

          } else {
            // there is a value to pass back out
            Done((res,
              {
                val cont = () => next(nextCont.asInstanceOf[ResumableIter[A,R]], s, toMany, true)
                val n = Cont( (i: Input[E]) => {
                  cont()
                })

                if (s().isEmpty) {
                  // need to process this res but force another to be
                  // calculated before it is returned to the enumerator
                  iteratee(n)
                } else {
                  // still data to process
                  cont()
                }
              }) , Empty[E])
          }
        )

      if (Eof.unapply(y) && !internalEOF) {
        // signal the end here to toMany, don't care about result
        toMany.foldT(done= (a1, y1) => false,
              cont = k => {
                k(Eof[E]); false
              })
      }

      returnThis
    }

    def next( i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]], internalEOF: Boolean = false ): ResumableIter[E, R] =

      i.foldT(
        done = (a, y) => doneWith(
          // oh for recursive types
          a.asInstanceOf[(R, ResumableIter[A, R])], y, i, s, toMany, internalEOF),
        cont =
          k => {
            contk(k, i, s, toMany)
          }
      )

    next(dest, empty, toMany)
  }

}
