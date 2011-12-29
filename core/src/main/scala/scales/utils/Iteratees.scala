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
  
}
