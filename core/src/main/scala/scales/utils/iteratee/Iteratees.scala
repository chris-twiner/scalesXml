package scales.utils.iteratee

import scalaz.{IterV, Enumerator, Input, EphemeralStream}
import scalaz.IterV._

import scales.utils.ScalesUtils
import scales.utils.io

/**
 * Evals once, the developer must check its Done, equivalent to a .run but
 * doesn't lose the continuation - no "Diverging Iteratee"
 */
trait Eval[WHAT,RETURN] {

  val orig : IterV[WHAT, RETURN]

  def eval : IterV[WHAT, RETURN] = {
    orig.fold(done = (x, y) => Done(x,y),
	 cont = k => k(EOF[WHAT]))
  }
}

trait IterateeImplicits {
  implicit def toEval[WHAT, RETURN]( i : IterV[WHAT, RETURN] ) = new Eval[WHAT, RETURN] {
    lazy val orig = i
  }

  /**
   * Taken from huynhjl's answer on StackOverflow, just abstracting the type to allow for better implicit inference
   */
  implicit val iteratorEnumerator = new Enumerator[Iterator] {
    @annotation.tailrec
    def apply[E,A](iter: Iterator[E], i: IterV[E,A]): IterV[E,A] = i match {
      case _ if iter.isEmpty => i
      case Done(acc, input) => i
      case Cont(k) =>
	val x : E = iter.next//.asInstanceOf[E]
      apply(iter, k(El(x)))
    }
  }

}

/**
 * Collection of iterateees
 */
trait Iteratees {

  /** drop while iteratee */
  def dropWhile[E](f: (E) => Boolean) : IterV[E, Option[E]] = {
    def step(s: Input[E]): IterV[E, Option[E]] =
      s(el = e => {
	if (f(e))
	  Cont(step)
	else
	  Done(Some(e), IterV.Empty[E])
	},
        empty = Cont(step),
        eof = Done(None, EOF[E]))
    Cont(step)
  }

  /** "find" iteratee, finds Some(first) or None */
  def find[E](f: (E) => Boolean) : IterV[E, Option[E]] =
    dropWhile(!f(_))

  /** filter iteratee, greedily taking all content until eof */
  def filter[E](f: (E) => Boolean) : IterV[E, Iterable[E]] = {
    def step(l : List[E])(s: Input[E]): IterV[E, Iterable[E]] =
      s(el = e => {
	if (f(e))
	  Cont(step(l :+ e))
	else
	  Cont(step(l))
	},
        empty = Cont(step(l)),
        eof = Done(l, EOF[E]))
    Cont(step(List()))
  }

  type ResumableIterList[E,A] = IterV[E, (Iterable[A],IterV[E,_])]
  type ResumableIter[E,A] = IterV[E, (A, IterV[E,_])]

  /**
   * Extract the continuation from a Done
   */
  def extractCont[E,A]( iter : ResumableIter[E, A] ) =
    iter.fold[ResumableIter[E,A]](
      done = (x, i) =>
	x._2.asInstanceOf[ResumableIter[E,A]],
      cont = f => error("Was not done") )

  /**
   * Extract the Some(value) from a Done or None if it was not Done.
   */
  def extract[E,A]( iter : ResumableIter[E, A] ) =
    iter.fold[Option[A]](
      done = (x, i) =>
	Some(x._1),
      cont = f => None )

  /**
   * Helper to identify dones
   */
  def isDone[E,A]( iter : IterV[E, A] ) =
    iter.fold[Boolean](
      done = (a, i) => true,
      cont = f => false )

  /**
   * Helper for done and empty
   */
  def isEmpty[E,A]( iter : IterV[E,A] ) =
    iter.fold[Boolean](
      done = (a, i) => IterV.Empty.unapply[E](i),
      cont = f => error("Iteratee is not Done") )

  /**
   * Helper for done and eof
   */
  def isEOF[E,A]( iter : IterV[E,A] ) =
    iter.fold[Boolean](
      done = (a, i) => EOF.unapply[E](i),
      cont = f => error("Iteratee is not Done") )

  /**
   * Converts a normal IterV[E,A] to a ResumableIter.
   *
   * Does so by folding over the iter once for an input
   * and when its Done starting again
   * with the original iter.  This is close to restarting the iter on
   * a new "stream", otherwise all attempts to keep the Cont will be made.
   */
  implicit def toResumableIter[E,A]( oiter : IterV[E,A]) : ResumableIter[E,A] = {
    def step(iter : IterV[E,A])( s : Input[E]) : ResumableIter[E, A] = {
      val next = iter match {// need to evaluate s in the case of done....
	  case Done(x, y) => Done((x, Cont(step(oiter))),y)
	  case Cont(k) =>
	    {
	      k(s) match {
		case i@Done(x, y) => Done((x, Cont(step(oiter))),y)
		case i@Cont(_) => Cont(step(i))
	      }
	    }
	}
      next.asInstanceOf[ResumableIter[E,A]]
    }

    Cont(step(oiter))
  }

  /**
   * Stepwise fold, each element is evaluated but each one is returned as a result+resumable iter.
   */
  def foldI[E,A]( f : (E,A) => A )( init : A ) : ResumableIter[E,A] = {
    def step( current : A )( s : Input[E] ) : ResumableIter[E,A] =
      s(el = {e =>
	val next = f(e,current)
	Done((next, Cont(step(next))), IterV.Empty[E])},
        empty = Cont(step(current)),
        eof = Done((current, Cont(step(init))),IterV.EOF[E]))
    Cont(step(init))
  }

  /**
   * Folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   * If there is a ping pong on enumerator -> Cont -> enumerator then we'll of course get an infinite loop.
   *
   * foldI returns a ResumableIter that performs a fold until a done, this folds over the enumerator to return a value.
   *
   * combine with onDone to get through chunks of data.
   */
  def foldOnDone[E,A, ACC, F[_]]( it : F[E] )( initAcc : ACC, initResumable : ResumableIter[E,A] )( f : (ACC, A) => ACC )(implicit e : Enumerator[F] ) : ACC = {
    import ScalesUtils._

    var currentI = initResumable(it).eval
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

      currentI = currentI(it).eval
      isdone = isDone(currentI)
    }
    currentA
  }

  class ResumableIterIterator[E,A,F[_]]( it : F[E])(init : ResumableIter[E,A])(implicit e : Enumerator[F]) extends Iterator[A] {
    import ScalesUtils._

    var cur = init(it).eval
    var isdone = isDone(cur)
    var r = extract(cur)

    def next = {
      val t = r
      cur = extractCont(cur)(it).eval
      isdone = isDone(cur)
      r = extract(cur)
      t.get // note we check for defined in hasNext
    }

    def hasNext = isdone && !isEOF(cur) && r.isDefined
  }

  /**
   * Converts the iteratee/enumerator/source triple into a Iterator
   */
  def withIter[E,A,F[_]]( it : F[E])(initResumable : ResumableIter[E,A])(implicit e : Enumerator[F]) = new ResumableIterIterator[E,A,F](it)(initResumable)

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

    def step(l : List[ResumableIter[E,A]])(s: Input[E]): ResumableIterList[E,A] =
      s(el = e => {
	var res : List[A] = Nil
	var newl : List[ ResumableIter[E,A] ] = Nil

	@inline def add( k : (scalaz.Input[E]) => scalaz.IterV[E,(A, scalaz.IterV[E, _])]) {
	  val d = k(El(e))
	  newl = d :: newl
	  d match {
	    case Done(x, _) => res = x._1 :: res
	    case _ => ()
	  }
	}

	val i = l.iterator
	while(i.hasNext) {
	  i.next match {
	  // safety first
	  case iter@Done(e1, _) =>
	    // are we EOF, in which case remove
	    if (isEOF(iter)) Nil
	    else {
	      if (isEmpty(iter))
		// feed back the continuation
		// this is where we hope that the users don't
		// break on purpose :-()
		e1._2 match {
		  case i @ Cont(k) => add(k.asInstanceOf[(scalaz.Input[E]) => scalaz.IterV[E,(A, scalaz.IterV[E, _])]])
		  case _ => error("Continuation can only be a Cont")
		}
	      else error("Can only handle EOF or Empty for Done")

	    }
	  case iter@Cont(k) => add(k)
	  }
	}

	if (res.isEmpty)
	  Cont(step(newl))
	else
	  Done((res, Cont(step(newl))), IterV.Empty[E])
	},
        empty = Cont(step(l)),
        eof = Done((Nil, Cont(step(l))), EOF[E]))
    Cont(step(originalList))
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
  def appendTo( to : Appendable ) : IterV[CharSequence, CharSequence] = {
    def step(s: Input[CharSequence]): IterV[CharSequence, CharSequence] =
      s(el = {e =>
	to.append(e)
	Done(e, IterV.Empty[CharSequence])},
        empty = Cont(step),
        eof = Done("",IterV.EOF[CharSequence]))
    Cont(step)
  }

  /**
   * Calls the function param with the fed data and returns its
   * result
   */
  def evalWith[FROM,TO]( f : (FROM) => TO ) : IterV[FROM, TO] = {
    def step(s: Input[FROM]): IterV[FROM, TO] =
      s(el = e => {
	val to = f(e)
	Done(to, IterV.Empty[FROM])
	},
        empty = Cont(step),
        eof = Cont(step))
    Cont(step)
  }

  /**
   * Enumeratee that converts input 1:1
   * String => Int, enumerator Iterator[String] but IterV[Int, Int]
   */
  def enumerateeMap[E, A, R]( dest : IterV[A,R])( f : E => A ) : IterV[E, R] = {

    def next( i : IterV[A,R] ) : IterV[E, R] =
      i.fold(
	done = (a, y) => Done(a, IterV.EOF[E]),
	cont = k => Cont((x: Input[E]) =>
	  x( el = e => next(k(IterV.El(f(e)))),
	    empty = next(k(IterV.Empty[A])),
	     eof = next(k(IterV.EOF[A])))
	)
      )

    next(dest)
  }

  /**
   * Sums an iteratee up
   */
  def sum[T](implicit n: Numeric[T]): IterV[T,T] = {
    import n._
    def step(acc: T)( s : Input[T] ) : IterV[T, T] =
      s( el = e => Cont(step(acc + e)),
	empty = Cont(step(acc)),
	eof = Done(acc, IterV.EOF[T])
      )
    Cont(step(zero))
  }

  /**
   * Maps a given input to a function returning a Input[EphemeralStream].
   * If the Input is El it returns it, if EOF empty and continues on empty.
   */
  def mapTo[E, A]( f: E => Input[EphemeralStream[A]] ): IterV[E, EphemeralStream[A]] = {
    def step(s: Input[E]): IterV[E, EphemeralStream[A]] =
      s( el = e => {
	    val r = f(e)
	    r( el = e1 => {
		  Done(e1, IterV.Empty[E])
		},
	      empty = Cont(step),
	      eof = Done(EphemeralStream.empty, IterV.EOF[E])
	    )
	  },
	empty = Cont(step),
	eof = Done(EphemeralStream.empty, IterV.EOF[E])
      )

    Cont(step)
  }

    /**
   * Enumeratee that folds over the Iteratee with Cont or Done and Empty, returning with Done and EOF.
   *
   * Converts ResumableIters on Done via a fold, returning Done only when receiving EOF from the initIter.
   *
   * NB - This can be thought of the reverse of toResumableIter but also accumulating.
   */
  def foldOnDoneIter[E,A, ACC]( initAcc : ACC, initIter : ResumableIter[E,A])( f : (ACC, A) => ACC ) : IterV[E, ACC] = {

    def next( acc : ACC, i : ResumableIter[E,A]) : IterV[E, ACC] =
      i.fold(
	done = (ac, y) => {
	  val (e, cont) = ac
	  val newacc = f(acc, e)
	  y match {
	    case IterV.El(a) =>
	      error("Cannot process an input element from Done")
	    case IterV.Empty() =>
	      // can continue
	      Cont( (x : Input[E]) =>
		// is the cont itself actually a done or a cont?
		next(newacc,
		     cont match {
		       case Done(x,y) => error("got a Done from a resumableIter cont "+ x +" "+y)
		       case Cont(k) =>
			 k(x).asInstanceOf[ResumableIter[E,A]]
		     }
		      ))
	    case IterV.EOF() => Done(newacc, IterV.EOF[E])
	  }
	},
	cont = k => Cont((x: Input[E]) =>
	  next(acc, k(x)))
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
    val empty = () => EphemeralStream.empty

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
	val (nc, ncs): (ResumableIter[A,R], EphemeralStream[A]) = c.fold(
	  done = (a, y) => (c, s()),// send it back, shouldn't be possible to get here anyway due to while test
	  cont =
	    k => {
	      // println("got cont")
	      val head = cs.head() // if used in El it captures the byname not the value
	      (k(IterV.El(head)), cs.tail())
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
    def pumpNext(x: Input[E], toMany: scalaz.Input[E] => ResumableIter[E,EphemeralStream[A]], k: scalaz.Input[A] => ResumableIter[A,R] ): ResumableIter[E, R] = {

      /*
       println("and then I was here "+
         x(el = e => e.toString,
         empty = "I'm empty ",
         eof = "I'm eof"))
       */
      val afterNewCall = toMany(x)
      //println("and then " + afterNewCall)

      afterNewCall.fold(
	done = (nextContPair, rest) => {
	  //println("was done wern't it")
	  val (e1, nextCont) = nextContPair
	  val nextContR = nextCont.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
	  if (isEOF(afterNewCall)) {
	    //println("after is eof")
	    next(k(IterV.EOF[A]), empty, nextContR)
	  } else {
	    if (e1.isEmpty) {
	      //println("empty on nextcontr")
	      next(k(IterV.Empty[A]), empty, nextContR)
	    }
	    else {
	      val h = e1.head()
	      // println("some data after all "+h)
	      next(k(IterV.El(h)), e1.tail, nextContR)
	    }
	  }
	},
	cont = k1 => {
	  //println("conted after here")
	  next(k(IterV.Empty[A]), empty, afterNewCall)
	}
      )
    }

    /**
     * For Cont handling we must loop when there is more data left on the stream,
     * when not verify if the toMany has returned more data to process.
     */
    def contk( k: scalaz.Input[A] => ResumableIter[A,R],  i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, R] = {
      if (!s().isEmpty) {
	val (ni, ns) = loop(i, s)
	//println("empty against the s "+ni + " " + ns().isEmpty)
	//if (isDone(ni))
	next(ni, ns, toMany) // bad - should let a done exit early
      } else
	Cont((x: Input[E]) =>
	  x( el = e => {
	    //println("got a cont x el e "+e)
	    toMany.fold (
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
	      next(k(IterV.Empty[A]), empty, toMany)
	    },
	    eof = {
	      next(k(IterV.EOF[A]), empty, toMany)
	    }
	  ))

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
	if ((isDone(nextCont) && isEOF(nextCont)) ||
	    (isDone(toMany) && isEOF(toMany))     || // either eof then its not restartable
	    (EOF.unapply(y) && !internalEOF )  // or the source is out of elements
	  ) {

	  Done((res, Done(res, IterV.EOF[E])), IterV.EOF[E])

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
		n
	      } else {
		// still data to process
		cont()
	      }
	    }), IterV.Empty[E])
	}

      if (EOF.unapply(y) && !internalEOF) {
	// signal the end here to toMany, don't care about result
	toMany.fold(done= (a1, y1) => false,
		    cont = k => {
		      k(IterV.EOF[E]); false
		    })
      }

      returnThis

    }

    def next( i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]], internalEOF: Boolean = false ): ResumableIter[E, R] =

      i.fold(
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
