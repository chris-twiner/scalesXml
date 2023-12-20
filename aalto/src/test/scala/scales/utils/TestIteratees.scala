package scales.utils

import scalaz.EphemeralStream
import scalaz.iteratee.Input.Eof
import scalaz.iteratee.Iteratee.{iteratee => siteratee}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{Enumerator, Input, Iteratee}
import scales.utils.{resource => sresource}

/**
 * Runs a previously eval'd continuation to completion - no "Diverging Iteratee" on the first iteration from a cont but may give "Confused Iteratee" when not finding an end state.  Its not in the standard utils as its dangerous and requires knowledge of both the enumerator and the data it enumerates.
 */
trait RunEval[WHAT,RETURN] {

  val orig : Iteratee[WHAT, RETURN]

  def runEval : RETURN = {
    var i = orig
    while(!isDone(i)) {
      i = i.foldT(done = (x, y) => siteratee(Done(x,y)),
	          cont = k => k(Eof[WHAT]))
    }
    i.foldT(done = (x, _) => x,
	    cont = k => error("Confused Iteratee!"))
  }
}

object DangerousIterateeImplicits {
  implicit def toRunEval[WHAT, RETURN]( i : Iteratee[WHAT, RETURN] ) = new RunEval[WHAT, RETURN] {
    lazy val orig = i
  }
}

object TestIteratees {
  /**
   * Drain through all, returning the last
   */
  def evalAll[FROM,TO](init : TO, f : (FROM) => TO ) : Iteratee[FROM, TO] = {
    def step(last : TO)(s: Input[FROM]): Iteratee[FROM, TO] =
      siteratee(
        s(el = e => {
            val to = f(e)
          //	println(" about to cont to")
            Cont(step(to)) // swallow them all
          },
            empty = {
        //	  println(" about to cont last")
            Cont(step(last))
          },
          eof = {
          //	  println(">>>>>>>> last is "+last)
            Done(last, Eof[FROM])
          }
        )
      )

    siteratee( Cont(step(init) _) )
  }

}
