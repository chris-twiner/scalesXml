package scales.utils
import ScalesUtils._
import scalaz.Free.Trampoline
import scalaz.iteratee.{EnumeratorT, IterateeT, StepT}
import scalaz.iteratee.Input.{Element, Empty, Eof, emptyInput, eofInput}
import scalaz.iteratee.Iteratee.{enumIterator, iterateeT => siteratee}
import scalaz.iteratee.StepT.{Cont, Done, scont}
import scalaz.Free._
import scalaz.{Bind, Monad}
import scalaz.Scalaz._

import scala.annotation.tailrec

class IterateeTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._

  import scalaz.iteratee.{Iteratee, Enumerator, Input}
  import scalaz.EphemeralStream
  import EphemeralStream.emptyEphemeralStream

  import scales.utils.{sum => usum}
  import scalaz.Scalaz._

  def F(implicit F: Monad[Trampoline]) = F

  def testSimpleEnumerateeMap(): Unit = {
    
    val i = List("1","2","3","4").iterator

    (enumerateeMap(usum[Int, Trampoline])( (s : String) => s.toInt ) &= iteratorEnumerator(i) ).run map
         { res =>
      assertEquals(10, res)
    } run
  }

  def testEnumToManyExhaustState(): Unit = {
    
    val l = List(1,2,3,4)

    def f(i: Int): ResumableIter[Int, Trampoline, EphemeralStream[Int]] = {
      def step(i: Int)(s: Input[Int]): ResumableIter[Int, Trampoline, EphemeralStream[Int]] =
        siteratee( F.point(
          s( el = e => {
              //println("i "+i+" e "+e)
              Done((iTo(i, e), siteratee( F.point( Cont(step(i + 1))) ) ), Input.Empty[Int])
            },
            empty = Cont(step(i)),
            eof = Done((emptyEphemeralStream, siteratee( F.point(  Cont(error("Shouldn't call cont on eof")).asInstanceOf[ResumableStep[Int, Trampoline, EphemeralStream[Int]]]) )), Eof[Int])
          ))
        )

      siteratee( F.point( Cont(step(i)) ))
    }

    val enum = (i: Int) => enumToMany(usum[Int, Trampoline])(f(i))

    val p =
      for {
        // 1, 2, 3, 4 only, the to is ignored
        r <- (enum(1) &= iteratorEnumerator(l.iterator)).run
        (res, cont: ResumableIter[Int, Trampoline, EphemeralStream[Int]]) = r
        is <- isDone(cont)
      } yield {
        assertEquals(10, res)

        assertTrue("should have been done", is)
      }

    p run

    val p2 =
      for {
        // -1->1 0, 0->2 3, 1->3 6, 2->4 9
        r <- (enum(-1) &= (iteratorEnumerator(l.iterator))).run
        (res2, cont2: ResumableIter[Int, Trampoline, EphemeralStream[Int]]) = r
        is <- isDone(cont2)
      } yield {
        assertEquals(18, res2)

        assertTrue("2 should have been done", is)
      }

    p2 run
  }

  def testEnumToManyExhaust(): Unit = {
    
    val i = List(1,2,3,4).iterator

    val p =
      for {
        r <- (enumToMany(usum[Int, Trampoline])( mapTo[Int, Trampoline, Int]( (i: Int) => Element(iTo(1, i)) ) ) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Int, Trampoline, EphemeralStream[Int]]) = r
        done <- isDone(cont)
      } yield {
        assertEquals(20, res)
        assertTrue("should have been done", done)
      }

    p.run
  }

  def testEnumToManyEOFFromMap = {
    
    val i = List(1,2,3,4).iterator

    val p =
      for {
        r <- (enumToMany(usum[Int, Trampoline])( mapTo[Int, Trampoline, Int]( (i: Int) => Eof[EphemeralStream[Int]] ) ) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Int, Trampoline, EphemeralStream[Int]]) = r
        done <- isDone(cont)
      } yield {
        assertEquals(0, res)
        assertTrue("should have been done", done)
      }

    p run

    val rest = i.take(4).toSeq
    //println(rest)
    assertEquals("Should still have had items in the list", 9, rest.sum)
  }

  /**
   * Make sure it doesn't soe in the loop
   */ 
  def testEnumToManySOE(): Unit = {
    val i = 1L to 5000L iterator//
      //List[Long](1,2,3,4,5).iterator

    val p =
      for {
        r <- (enumToMany(usum[Long, Trampoline])(mapTo[Long, Trampoline, Long]((_: Long) => Element(lTo(1L, 100L)))) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Long, Trampoline, Long]) = r
        done <- isDone(cont)
      } yield {
        assertEquals (25250000L, res)
        assertTrue ("should have been done", done)
      }

    p.run
  }

  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, iTo(lower + 1, upper))


  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // force a restart at magic number 1
    val sum: ResumableIter[Long,Trampoline, Long] = {
      def step(acc: Long)( s : Input[Long] ) : ResumableIter[Long, Trampoline, Long] =
        siteratee(F.point(
          s( el = e => {
              val nacc = acc + e
              if (nacc == 25000050001L)
                Done((nacc, siteratee( F.point( Cont(step(nacc)) )) ), Empty[Long])
              else
                Cont(step(nacc))
            },
            empty = Cont(step(acc)),
            eof = Done((acc, siteratee( F.point( Done(acc, Empty[Long]).asInstanceOf[ResumableStep[Long, Trampoline, Long]] ))), Eof[Long])
          )
        ))

      siteratee( F.point( Cont(step(0))) )
    }

    val p =
      for {
        r <- (enumToMany(sum) (mapTo[Long, Trampoline, Long]((_: Long) => Element(lTo(1L, 100000L)))) &= iteratorEnumerator (i)).run
        (res, cont: ResumableIter[Long, Trampoline, Long]) = r
        r2 <- (cont).run
        (res2, cont2: ResumableIter[Long, Trampoline, Long]) = r2
        done <- isDone(cont)
        done2 <- isDone(cont2)
      } yield {
        assertEquals (25000050001L, res)
        assertFalse ("should not have been done", done)

        assertEquals(25000250000L, res2)
        assertTrue("should have been done", done2)
      }

    p run
  }

  def testEveryItem(): Unit = {
    val source = List[Long](1,4,7,10,13).iterator

    type TheF[X] = Id[X]
    def F(implicit F: Monad[TheF]) = F

    // force a restart every entry
    val echo: ResumableIter[Long,TheF, Long] = {
      def step( s : Input[Long] ) : ResumableIter[Long, TheF, Long] =
        siteratee( F.point(
          s( el = e => {
        //	  println("got "+e)
              //if (e > 2) {
                Done((e, siteratee( F.point( Cont(step) ))), Empty[Long])
              //} else Cont(step)
              //
            },
            empty = Cont(step),
            eof = //Done((0, siteratee( F.point( Done(0, Eof[Long])))), Eof[Long])
              resumableEOFDone(0)
              //Done((0, siteratee( F.point( Cont(step) ))), Eof[Long])
          )
        ))

      siteratee( F.point( Cont(step) ) )
    }
/*
    type Triple = (Long, Int, Int)
    type Quad = (Triple, ResumableIter[Long, TheF, Long])

    def loop(iter: ResumableIter[Triple, TheF, Triple]):ResumableIter[Quad, TheF, Quad] = {
      def step(istep: ResumableStep[Quad,TheF, Quad]): ResumableIter[Quad, TheF, Quad] =
        siteratee(
          istep(
            done = (nextContPair, rest) => {
              val (((res, count, i), itr), nextCont) = nextContPair
              val nextContR = nextCont.asInstanceOf[ResumableIter[Quad, TheF, Quad]]
              val rrest = if (rest.isEof) eofInput[Quad] else emptyInput[Quad]
              if (i == 15)
                F.point(Done((((res, count, i), itr), nextContR), Eof[Quad]))
              else {
                F.bind(isResumableEOF(nextContR)) {
                  isEOF =>

    //                assertEquals(i, res)
                  val cres = itr.eval
                  F.bind(cres.value) {
                    s =>
                      if (isDone(s) && !isEOF) {
                        F.bind(extract(cres)) {
                          ol =>
                            val newres = ol.get
                            val newiter = extractCont(cres)
                            val ntriple = newres.copy(_3 = newres._3 + 1)
                            Done(((ntriple, newiter), loop(newiter)), rrest)
                        }
                      } else {
                        val newcount = count + 1
                        // run on the source - as we need more data
                        val newstep =
                          for {
                            r <- (cres &= iteratorEnumerator(source.map(r => (r, newcount, i)))).run
                            (newres, ncont) = r
                          } yield {
                            if (count == 6) {
                              assertEquals("should have pushed the eof", 15, i)
                            }
                            Done((newres.copy(_3 = newres._3 + 1), ncont.asInstanceOf[ResumableIter[Quad, TheF, Quad]]), rrest).asInstanceOf[ResumableStep[Quad, TheF, Quad]]
                          }

                        newstep
                      }
                  }

                }
              }
            },
            cont = k => F.point(Cont(in => k(in) >>== step))
          )
        )

      // add the iter in
      siteratee[Quad, TheF, (Quad, ResumableIter[Quad, TheF, Quad])](F.bind(iter.value)( s => s(
        done = (i, y) => {
          val (a, cont: ResumableIter[Triple, TheF, Triple]) = i
          F.point( Done(((a, cont), loop(iter)), y(
            el = e => Element((e, iter)),
            empty = emptyInput[Quad],
            eof = eofInput[Quad]
          )) )
        },
        cont = k => loop( siteratee( F.point( scont(k) ))).value.asInstanceOf[TheF[StepT[Quad, TheF, (Quad, ResumableIter[Quad, TheF, Quad])]]]
      ))) >>== {s => step(s.asInstanceOf[ResumableStep[Quad, TheF, Quad]]) }
    }
    */

    val oitr = enumToMany(echo)(mapTo[Long, TheF, Long] {
          (x: Long) =>
            //println("evaled to "+x);
            Element(lTo(x, x + 2L))
        }) &= iteratorEnumerator(source)
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

    val lifted =
      resumableMap(resumableContramap[Triple, Long, TheF, Long](oitr)(r => r._1))(r => (r, 1, 1))
*/

    val p =
      for {
        //or <- lifted.run
        or <- oitr.run
        (origres, origcont: ResumableIter[Long, TheF, Long]) = or

        _ = assertEquals(1, origres)
        //list = Seq(((origres, 1, 1),oitr)

        r <- (foldIM[Int, TheF, (Long, Int, ResumableIter[Long, TheF, Long])]{ (i, quad) =>
          val (ores, ocount, itr) = quad
          var res = ores
          var iter = itr
          var count = ocount

          //assertEquals(i, res)
          val cres : ResumableIter[Long, TheF, Long] = iter.eval

          F.map(cres.value) { cresStep =>

            if (isDoneS(cresStep)/* && !isResumableEOF(cresStep)*/) {
              res = extract(cres).get
              iter = extractCont(cres)
            } else {
              //	println(">>><<<<>>>>><<<<")
              count += 1
              // run on the source - as we need more data
              val r: Any = (cres &= iteratorEnumerator(source)).run
              val (nres: Long, ncont: ResumableIter[Long, TheF, Long]) = r
              res = nres
              iter = ncont

              if (count == 6) {
                assertEquals("should have pushed the eof", 15, i)
              }
            }

            (res, count, iter)
          }
        }((origres, 1, origcont)) &= iteratorEnumerator[Int, TheF]((1 to 15).iterator)).run

        ((res, count, itr: ResumableIter[Long, TheF, Long]), last) = r

        done = isDone(itr)
        eof = isEOF(itr)
      } yield {
      assertEquals(0, res)
      assertTrue("should have been done", done)
      assertTrue("should have been eof", eof)
      assertFalse("source should be empty", source.hasNext)
      assertEquals("should have reset 6 times - once for start and once for EOF ", 6, count)
    }

    p// run
  }

}
