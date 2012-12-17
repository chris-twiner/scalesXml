package scales.utils

import scalaz._
import scalaz.IterV._
import Scalaz._

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
   * This version of enumToMany returns Done((None, cont), Empty) when the toMany iteratee cannot supply anything else than Empty for an Empty input.
   */ 
  def enumToManyAsync[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, io.AsyncOption[R]] = 
    enumToManyAsyncOption[E, A, R, io.AsyncOption[R]](
      identity, // keep the option
      true // should send us the option
      )(dest)(toMany)
  
  /**
   * Defaults to continuing when Empty is returned by toMany for an Empty input.
   */ 
  def enumToMany[E, A, R]( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R] = 
    enumToManyAsyncOption[E, A, R, R](
      _.getOrElse(error("No Asynchnronous Behaviour Expected But the toMany still recieved an Empty and returned a Done Empty")), // throw if async somehow got returned
      false // use cont instead
      )(dest)(toMany)
  
  /**
   * Takes a function f that turns input into an Input[EphemeralStream] of a different type A.  The function f may return El(EphemeralStream.empty) which is treated as Empty.
   * This function must return an ResumableIter in order to capture early Done's without losing intermediate chunks,
   * the destination iter having the same requirements.
   *
   * Option is required in the return to handle the case of empty -> empty infinite loops.  For asynchronous parsing, for example, we should be able to return an empty result but with Empty as the input type.
   */ 
  def enumToManyAsyncOption[E, A, R, T](converter: io.AsyncOption[R] => T, doneOnEmptyForEmpty: Boolean)( dest: ResumableIter[A,R])( toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, T] = {
    val empty = () => EphemeralStream.empty

    def loop( i: ResumableIter[A,R], s: () => EphemeralStream[A] ):
      (ResumableIter[A,R], () => EphemeralStream[A]) = {
      var c: ResumableIter[A,R] = i
      var cs: EphemeralStream[A] = s() // need it now
//println("loopy")
      while(!isDone(c) && !cs.isEmpty) {
//	println("doing a loop")
	val (nc, ncs): (ResumableIter[A,R], EphemeralStream[A]) = c.fold(
	  done = (a, y) => (cs, s()),// send it back
	  cont = 
	    k => {
	      val head = cs.head() // if used in El it captures the byname not the value
	      (k(IterV.El(head)), cs.tail())
	    }
	    )
	c = nc
	cs = ncs
      }
      (c, () => cs)
    }

    def next( i: ResumableIter[A,R], s: () => EphemeralStream[A], toMany: ResumableIter[E, EphemeralStream[A]] ): ResumableIter[E, T] =
      i.fold(
	done = (a, y) => {
//	  println(" y is "+y) 

	  val (rawRes, nextCont) = a
	  val res = converter(io.HasResult(rawRes))

	  val returnThis : ResumableIter[E, T] = 
	  if ((isDone(nextCont) && isEOF(nextCont)) ||
	      (isDone(toMany) && isEOF(toMany))     || // either eof then its not restartable
	      (EOF.unapply(y)) // or the source is out of elements
	      ) { 
	    Done((res, Done(res, IterV.EOF[E])), IterV.EOF[E])
	  } else {
	    Done((res, 
	      {
		val n = next(nextCont.asInstanceOf[ResumableIter[A,R]], s, toMany)
	      
		if (s().isEmpty)
		  Done((res, n), IterV.Empty[E])
		else
		  n
	      }), IterV.Empty[E])
	  }

	  if (EOF.unapply(y)) {
	    // signal the end here to toMany, don't care about result
	    toMany.fold(done= (a1, y1) => false,
			cont = k => {
			  k(IterV.EOF[E]); false
			})
	  }
	  
	  returnThis
	  },
	cont = 
	  k => {
	    if (!s().isEmpty) {
	      val (ni, ns) = loop(i, s)
	      next(ni, ns, toMany)
	    } else
	      Cont((x: Input[E]) => 
		x( el = e => {
		  //println("got a "+e)
		  toMany.fold (
		    done = (a, y) => {
		      val (e1, nextContR) = a
		      val nextCont = nextContR.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
		      error("Unexpected State for enumToMany - Cont but toMany is done")		     	
		    },
		    cont = y => {
		      val afterNewCall = y(x)
		      afterNewCall.fold(
			done = (nextContPair, rest) => {
			  val (e1, nextCont) = nextContPair
			  val nextContR = nextCont.asInstanceOf[ResumableIter[E,scalaz.EphemeralStream[A]]]
			  if (isEOF(afterNewCall)) {
			    next(k(IterV.EOF[A]), empty, nextContR)
			  } else {
			    if (e1.isEmpty) {
			      //println("empty on nextcontr")
			      next(k(IterV.Empty[A]), empty, nextContR)
			    }
			    else
			      next(k(IterV.El(e1.head())), e1.tail, nextContR)
			  }
			},
			cont = k1 => {
			  next(k(IterV.Empty[A]), empty, afterNewCall)
			}
			)
		    }
		  )
		},
		  empty = {
		    //println("empty on cont")
		    toMany.fold (
		      done = (a, y) => {
			error("shouldn't be done ever, unless it was done to start with")
		      },
		      cont = y => {
			//println("looping back again, the to many can't do anything with empty")
			// push it through
			val res = y(x)
			res.fold (
			  done = (a1, y1) => {
			    val (e1 : EphemeralStream[A], nextCont : ResumableIter[E,EphemeralStream[A]]) = a1.asInstanceOf[(EphemeralStream[A], ResumableIter[E,EphemeralStream[A]])]
			    if (doneOnEmptyForEmpty && e1.isEmpty && IterV.Empty.unapply[E](y1)) {
			      // the toMany has indicated it can't do anything more
			      // don't loop but drop out
			      //println("drop out")
			      Done((converter(io.NeedsMoreData), 
				    next(k(IterV.Empty[A]), empty, nextCont)), IterV.Empty[E])	
			    }
			    else {
			      //println("couldn't drop out ")
			      next(k(IterV.Empty[A]), () => e1, nextCont)
			    }   
			  },
			  cont = kn => next(k(IterV.Empty[A]), empty, res)
			)
		      }
		    )
		  },
		  eof = next(k(IterV.EOF[A]), empty, toMany)
		))
	  }
      )

    next(dest, empty, toMany)
  }

}
