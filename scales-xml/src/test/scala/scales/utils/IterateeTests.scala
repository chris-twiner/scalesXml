package scales.utilsTest
import scales.utils.ScalesUtils._
import scales.utils.WeakStream
import junit.framework.Assert.{assertTrue, fail}
import scalaz.EphemeralStream.emptyEphemeralStream
import scalaz.Free.Trampoline
import scalaz.iteratee.{EnumeratorT, IterateeT, StepT}
import scalaz.iteratee.Input.{Element, Empty, Eof, emptyInput, eofInput}
import scalaz.iteratee.Iteratee.{done, empty, enumEofT, enumIterator, eofInput, foldM, iterateeT}
import scalaz.iteratee.StepT.{Cont, Done, scont}
import scalaz.Free._
import scalaz.{Applicative, Bind, EphemeralStream, Monad}
import scalaz.Scalaz._
import scalaz.effect.IO
import scales.utilsTest.IterateeTests.{enumWeakStreamF, isDoneT, maxIterations}
import scales.utils.iteratee.Eval
import scales.utils._
import scala.annotation.tailrec
import scales.utils.iteratee.functions.ResumableIterOps
object StreamHelpers {

  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, iTo(lower + 1, upper))


}

object IterateeTests {

  def enumWeakStream[E, F[_] : Monad](xs: WeakStream[E]): EnumeratorT[E, F] = {
    import WeakStream.##::

    new EnumeratorT[E, F] {
      def apply[A] = (s: StepT[E, F, A]) =>
        s(done = (x,y) => {
          val isEof = y.isEof
          val isEL = y.isEl
          val isEmpty = y.isEmpty
          if (xs.isEmpty)
            done(x, Eof[E])
          else
            s.pointI
        }
          ,
          cont =
            k =>
              if (xs.nonEmpty) xs match {
                case h ##:: t => s.mapCont(k => k(scalaz.iteratee.Iteratee.elInput(h)) >>== enumWeakStream[E, F](t).apply[A])
                case _       => s.pointI
              } else
                s.pointI
        )
    }
  }


  def enumWeakStreamF[E, F[_] : Monad](state: WeakStream[E] => Unit)(xs: WeakStream[E]): EnumeratorT[E, F] = {
    import WeakStream.##::

    new EnumeratorT[E, F] {
      def apply[A] = (s: StepT[E, F, A]) =>
        s(done = (x,y) => {
          val isEof = y.isEof
          val isEL = y.isEl
          val isEmpty = y.isEmpty
          if (xs.isEmpty)
            done(x, Eof[E])
          else
            s.pointI
        }
          ,
          cont =
            k =>
              if (xs.nonEmpty) xs match {
                case h ##:: t => s.mapCont(k => k(scalaz.iteratee.Iteratee.elInput(h)) >>== enumWeakStreamF[E, F]({state(t); state})(t).apply[A])
                case _       => s.pointI
              } else
                s.pointI
        )
    }
  }

  import scales.utils.iteratee.functions._

  def isDoneT[E,F[_],A]( i : Int, res : ResumableIter[E,F,A])(test: A => Unit)(implicit F: Monad[F]) =
    Monad[F].map(res.value) { step =>
      step(
        done = (a, y) => {
          val (x, cont) = a
          test(x)
          if (i == maxIterations)
            assertTrue("should have been Eof " + i, y.isEof)
          else
            assertTrue("should have been Empty " + i, y.isEmpty)
        },
        cont = _ => fail("was not done " + i)
      )
    }

  /*
   * pump up number when cold, but make sure its even.
   *
   * 5000000 takes around 185s (as of 14.01.2010) and shows no leaking/unneccesary retention.
   */
  val maxIterations = 500//0000

}

class IterateeTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._

  import StreamHelpers._
  import scalaz.iteratee.{Iteratee, Enumerator, Input}
  import scalaz.EphemeralStream
  import EphemeralStream.emptyEphemeralStream

  import scalaz.Scalaz._
  import scales.utils.trampolineIteratees._

  def testSimpleEnumerateeMap(): Unit = {
    
    val i = List("1","2","3","4").iterator

    (enumerateeMap(sum[Int])( (s : String) => s.toInt ) &= iteratorEnumerator(i) ).run map
         { res =>
      assertEquals(10, res)
    } run
  }

  def testEnumToManyExhaustState(): Unit = {

    val l = List(1,2,3,4)

    def f(i: Int): ResumableIter[Int, EphemeralStream[Int]] = {
      def step(i: Int)(s: Input[Int]): ResumableIter[Int, EphemeralStream[Int]] =
        iterateeT( F.point(
          s( el = e => {
              //println("i "+i+" e "+e)
              Done((iTo(i, e), iterateeT( F.point( Cont(step(i + 1))) ) ), Input.Empty[Int])
            },
            empty = Cont(step(i)),
            eof = Done((emptyEphemeralStream, iterateeT( F.point(  Cont(error("Shouldn't call cont on eof")).asInstanceOf[ResumableStep[Int, EphemeralStream[Int]]]) )), Eof[Int])
          ))
        )

      iterateeT( F.point( Cont(step(i)) ))
    }

    val enum = (i: Int) => enumToMany(sum[Int].toResumableIter)(f(i))

    val p =
      for {
        // 1, 2, 3, 4 only, the to is ignored
        r <- (enum(1) &= iteratorEnumerator(l.iterator)).run
        (res, cont: ResumableIter[Int, EphemeralStream[Int]]) = r
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
        (res2, cont2: ResumableIter[Int, EphemeralStream[Int]]) = r
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
        r <- (enumToMany(sum[Int].toResumableIter)( mapTo[Int, Int]( (i: Int) => Element(iTo(1, i)) ).toResumableIter ) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Int, EphemeralStream[Int]]) = r
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
        r <- (enumToMany(sum[Int].toResumableIter)( mapTo[Int, Int]( (i: Int) => Eof[EphemeralStream[Int]] ).toResumableIter ) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Int, EphemeralStream[Int]]) = r
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
        r <- (enumToMany(sum[Long].toResumableIter)(mapTo[Long, Long]((_: Long) => Element(lTo(1L, 100L))).toResumableIter) &= iteratorEnumerator(i)).run
        (res, cont: ResumableIter[Long, Long]) = r
        done <- isDone(cont)
      } yield {
        assertEquals (25250000L, res)
        assertTrue ("should have been done", done)
      }

    p.run
  }

  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // force a restart at magic number 1
    val sum: ResumableIter[Long,Long] = {
      def step(acc: Long)( s : Input[Long] ) : ResumableIter[Long,Long] =
        iterateeT(F.point(
          s( el = e => {
              val nacc = acc + e
              if (nacc == 25000050001L)
                Done((nacc, iterateeT( F.point( Cont(step(nacc)) )) ), Empty[Long])
              else
                Cont(step(nacc))
            },
            empty = Cont(step(acc)),
            eof = Done((acc, iterateeT( F.point( Done(acc, Empty[Long]).asInstanceOf[ResumableStep[Long, Long]] ))), Eof[Long])
          )
        ))

      iterateeT( F.point( Cont(step(0))) )
    }

    val p =
      for {
        r <- (enumToMany(sum) (mapTo[Long, Long]((_: Long) => Element(lTo(1L, 100000L))).toResumableIter) &= iteratorEnumerator (i)).run
        (res, cont: ResumableIter[Long, Long]) = r
        r2 <- cont.run
        (res2, cont2: ResumableIter[Long, Long]) = r2
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

    // force a restart every entry
    val echo: ResumableIter[Long, Long] = {
      def step( s : Input[Long] ) : ResumableIter[Long, Long] =
        iterateeT( F.point(
          s( el = e => {
        //	  println("got "+e)
              //if (e > 2) {
                Done((e, iterateeT( F.point( Cont(step) ))), Empty[Long])
              //} else Cont(step)
              //
            },
            empty = Cont(step),
            eof = //Done((0, iterateeT( F.point( Done(0, Eof[Long])))), Eof[Long])
              resumableEOFDone(0)
              //Done((0, iterateeT( F.point( Cont(step) ))), Eof[Long])
          )
        ))

      iterateeT( F.point( Cont(step) ) )
    }

    val oitr = enumToMany(echo)(mapTo[Long, Long] {
          (x: Long) =>
            Element(lTo(x, x + 2L))
        }.toResumableIter) &= iteratorEnumerator(source)

    val p =
      for {
        or <- oitr.run
        (origres, origcont: ResumableIter[Long, Long]) = or

        _ = assertEquals(1, origres)

        r <- (foldM[Int, TheF, (Long, Int, ResumableIter[Long, Long])]((origres, 1, origcont)){ (quad, i) =>
          //println("loop !!!! "+i)
          val (res, count, itr) = quad

          assertEquals(i, res)
          val cres : ResumableIter[Long, Long] = itr.eval

          F.bind(cres.value) { cresStep =>

            if (isDoneS(cresStep)) {
              val fo = extract(cres)
              F.map(fo) {
                o =>
                  (o.get, count, extractCont(cres))
              }
            } else {
              //	println(">>><<<<>>>>><<<<")
              val ncount = count + 1
              // run on the source - as we need more data
              for {
                r <- (cres &= iteratorEnumerator(source)).run
                (nres: Long, ncont: ResumableIter[Long,  Long]) = r
              } yield {
                if (count == 6) {
                  assertEquals("should have pushed the eof", 15, i)
                }
                (nres, ncount, ncont)
              }
            }
          }
        } &= iteratorEnumerator[Int]((1 to 15).iterator)).run

        (res, count, itrr) = r
        itr = itrr.asInstanceOf[ scalaz.iteratee.IterateeT[Long, TheF,(Long, ResumableIter[Long,Long])] ]

        done <- isDone(itr)
        eof <- isEOF(itr)
      } yield {
      assertEquals(0, res)
      assertTrue("should have been done", done)
      assertTrue("should have been eof", eof)
      assertFalse("source should be empty", source.hasNext)
      assertEquals("should have reset 6 times - once for start and once for EOF ", 6, count)
    }

    p run
  }

  /**
   * Normal iters can't maintain state if they return Done, since
   * we pass back a new iter as well we can keep state
   */
  def testResumableIterFolds(): Unit = {
    //val liter = (1 to maxIterations).toIterator
    var liter = WeakStream.iterTo( 1 to maxIterations iterator )//WeakStream.iTo(1, maxIterations)

    import scales.utils.ioIteratees._

    val func = (s : WeakStream[Int]) => {liter = s}

    //def enum(i: Iterator[Int]) = iteratorEnumerator[Int, F](i)
    def enum(i: WeakStream[Int]) = enumWeakStreamF[Int, TheF](func)(i)
    /*
        def enum(i: WeakStream[Int]) =
          //enumIndexedSeq2[Int, F](i.toIterator.toIndexedSeq)
          //iteratorEnumerator[Int,F](i)
          //enumEphemeralStream[Int, F](i)
          enumWeakStream[Int, F](i)
    */
    type ITER = ResumableIter[Int, Long]

    val counter = runningCount[Int]

    def isDone( i : Int, res : ITER) =
      isDoneT(i, res){ x =>
        assertEquals(i, x)
      }

    val starter = (counter &= enum(liter)).eval
    /* Trampoline */

    val p =
      for {
        _ <- isDone(1, starter)

        // check it does not blow up the stack and/or mem
        r <- (foldM[Int, TheF, ITER](starter){ (itr, i) =>
          F.bind(itr.value){
            _ =>
              val nitr = (extractCont(itr) &= enum(liter)).eval
              F.bind(nitr.value) {
                _ =>
                  val rc = isDone(i, nitr)
                  F.map(rc) { _ =>
                    nitr
                  }
              }
          }
        } &= iteratorEnumerator((2 to maxIterations).iterator) ) run

        vf <- extract(r)
        v = vf
        rstep <- r.value
        isdone = isDoneS(rstep)
        iseof = isEOFS(rstep)
        isempty = isEmptyS(rstep)
        res = (extractCont(r) &= enum(liter)).eval

        step <- res.value
      } yield {
        step(
          done = (a, y) => {
            val (x, cont) = a

            assertTrue("should have been EOF", y.isEof)
            assertEquals(maxIterations, x)
          },
          cont = _ => fail("was not done")
        )
      }

    p.unsafePerformIO()
  }

  /**
   * Test two resumables next to each other, there should always be one done, the count
   * and another that is done only each three.
   */
  def testResumableOnDone():Unit = {
    val counter = runningCount[Int]

    var liter = WeakStream.iterTo((1 to maxIterations).iterator)
    val func = (s : WeakStream[Int]) => {liter = s}

    def enum(i: WeakStream[Int]) = enumWeakStreamF[Int, Trampoline](func)(i)

    def step( list : List[Long])( s : Input[Int] ) : ResumableIter[Int, Long] =
      iterateeT(F.point(
        s(el = {e =>
          val next = e.longValue :: list
          if (next.size == 3)
            Done((e, iterateeT(F.point( Cont(step(List()))))), Input.Empty[Int])
          else
            Cont(step(next))
        },
          empty = Cont(step(list)),
          eof = Done((list.last, iterateeT(F.point( Cont(step(List()) )))), Eof[Int])
        )
      ))

    val inThrees = iterateeT( F.point( Cont(step(List())) ) )

    val ionDone = onDone[Int, Long](List(counter, inThrees))

    def isDone( i : Int, res : ResumableIterList[Int, Long]) =
      F.map(res.value){ step =>
        step(
          done = (x,y) => (x,y) match {
            case ((x :: Nil,cont), y) if i % 3 != 0 =>
              assertEquals(i, x)
              assertTrue("should have been Empty "+i, y.isEmpty)
            case ((x :: x2 :: Nil,cont), y)  if i % 3 == 0 =>
              assertEquals(i, x)
              assertEquals(i, x2)
              assertTrue("should have been Empty "+i, y.isEmpty)
            case (x,y) => fail("Was done but not expected "+ x +" -> " + y )

          },
          cont = _ => fail("was not done "+i))
      }

    val starter = (ionDone &= enum(liter)).eval

    val p =
      for {
        _ <- isDone(1, starter)

        // check it does not blow up the stack and/or mem
        r <- (foldM[Int, Trampoline, ResumableIterList[Int,Long]](starter){ (itr, i) =>
          val nitr = (extractCont(itr) &= enum(liter)).eval
          Monad[Trampoline].map(nitr.value){
            _ =>
              isDone(i, nitr)
              nitr
          }
        } &= iteratorEnumerator((2 to maxIterations iterator))) run

        res = (extractCont(r) &= enum(liter)).eval

        step <- res.value
      } yield {
        step(
          done = (x,y) =>
            (x,y) match {
              case ((Nil,cont), y)  =>
                assertTrue("should have been EOL", y.isEof)

            },
          cont = _ => fail("was not done with empty")
        )
      }

  }


  def testResumableIterConversion = {
    val liter = (1 to maxIterations).iterator

    type ITER = ResumableIter[Int, Option[Int]]

    val itrOdd : ITER  = find[Int]( (x : Int) => x % 2 == 1 ).toResumableIter

    def isDone( i : Int, res : ITER) =
      isDoneT(i, res){ x =>
        assertTrue("is defined " + i, x.isDefined)
        assertEquals(i, x.get)
      }

    val starter = (itrOdd &= iteratorEnumerator(liter)).eval

    val p =
      for {
        _ <- isDone(1, starter)

        // check it does not blow up the stack and/or mem
        r <- (foldM[Int, Trampoline, ITER](starter){ (itr, i) =>
          val nitr = (extractCont(itr) &= iteratorEnumerator(liter)).eval
          Monad[Trampoline].map(nitr.value){
            _ =>
              isDone(i, nitr)
              nitr
          }
        } &= iteratorEnumerator((2 to maxIterations).iterator.filter(_ % 2 == 1)) ) run

        res = (extractCont(r) &= iteratorEnumerator(liter)).eval
        step <- res.value
      } yield {
        step(
          done = (a, y) =>
          {
            val (x,cont) = a
            assertFalse("should not be defined", x.isDefined)
            assertTrue("should have been EOF", isEOFS(step))
          },
          cont = _ => fail("was not done")
        )
      }

    p run
  }
}
