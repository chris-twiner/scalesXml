package scales.utils
import ScalesUtils._
import junit.framework.Assert.{assertTrue, fail}
import scalaz.EphemeralStream.emptyEphemeralStream
import scalaz.Free.Trampoline
import scalaz.iteratee.{EnumeratorT, IterateeT, StepT}
import scalaz.iteratee.Input.{Element, Empty, Eof, emptyInput, eofInput}
import scalaz.iteratee.Iteratee.{done, enumIterator, foldM, iterateeT => siteratee}
import scalaz.iteratee.StepT.{Cont, Done, scont}
import scalaz.Free._
import scalaz.{Bind, EphemeralStream, Monad}
import scalaz.Scalaz._
import scalaz.effect.IO
import scales.utils.IterateeTests.{isDoneT, maxIterations}

import scala.annotation.tailrec

object StreamHelpers {

  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, iTo(lower + 1, upper))


}

object IterateeTests {

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

      /*xs match {
      case h ##:: t => s.mapCont(k => k(scalaz.iteratee.Iteratee.elInput(h)) >>== enumWeakStream[E, F](t).apply[A])
      case _       => s.pointI
    }*/
    }
  }

  def isDoneT[E,F[_],A]( i : Int, res : ResumableIter[E,F,A])(test: A => Unit)(implicit F: Monad[F]) =
    Monad[F].map(res.value) { step =>
      step(
        done = (a, y) => {
          val (x, cont) = a
          test(x)
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
        r2 <- cont.run
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

    type TheF[X] = Trampoline[X]
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

    val oitr = enumToMany(echo)(mapTo[Long, TheF, Long] {
          (x: Long) =>
            Element(lTo(x, x + 2L))
        }) &= iteratorEnumerator(source)

    val p =
      for {
        or <- oitr.run
        (origres, origcont: ResumableIter[Long, TheF, Long]) = or

        _ = assertEquals(1, origres)

        r <- (foldM[Int, TheF, (Long, Int, ResumableIter[Long, TheF, Long])]((origres, 1, origcont)){ (quad, i) =>
          //println("loop !!!! "+i)
          val (res, count, itr) = quad

          assertEquals(i, res)
          val cres : ResumableIter[Long, TheF, Long] = itr.eval

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
                (nres: Long, ncont: ResumableIter[Long, TheF, Long]) = r
              } yield {
                if (count == 6) {
                  assertEquals("should have pushed the eof", 15, i)
                }
                (nres, ncount, ncont)
              }
            }
          }
        } &= iteratorEnumerator[Int, TheF]((1 to 15).iterator)).run

        (res, count, itrr) = r
        itr = itrr.asInstanceOf[ scalaz.iteratee.IterateeT[Long,TheF,(Long, scales.utils.ResumableIter[Long,TheF,Long])] ]

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
    //val liter = (1 to maxIterations)
    val liter = WeakStream.iterTo( 1 to maxIterations iterator )//WeakStream.iTo(1, maxIterations)

    type F[X] = IO[X]

    def enum(i: WeakStream[Int]) =
      IterateeTests.enumWeakStream[Int, F](i)
    /*
        def enum(i: WeakStream[Int]) =
          //enumIndexedSeq2[Int, F](i.toIterator.toIndexedSeq)
          //iteratorEnumerator[Int,F](i)
          //enumEphemeralStream[Int, F](i)
          enumWeakStream[Int, F](i)
    */
    type ITER = ResumableIter[Int, F, Long]

    val counter = runningCount[Int, F]

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
        r <- (foldM[Int, F, ITER](starter){ (itr, i) =>
          Monad[F].bind(itr.value){
            _ =>
              val nitr = (extractCont(itr) &= enum(liter)).eval
              Monad[F].bind(nitr.value) {
                _ =>
                  val rc = isDone(i, nitr)
                  Monad[F].map(rc) { _ =>
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
            assertEquals(maxIterations + 1, x)
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
    val liter = (1 to maxIterations).iterator

    val counter = runningCount[Int, Trampoline]

    val F = implicitly[ Monad[Trampoline] ]

    def step( list : List[Long])( s : Input[Int] ) : ResumableIter[Int, Trampoline, Long] =
      siteratee(F.point(
        s(el = {e =>
          val next = e.longValue :: list
          if (next.size == 3)
            Done((e, siteratee(F.point( Cont(step(List()))))), Input.Empty[Int])
          else
            Cont(step(next))
        },
          empty = Cont(step(list)),
          eof = Done((list.last, siteratee(F.point( Cont(step(List()) )))), Eof[Int])
        )
      ))

    val inThrees = siteratee( F.point( Cont(step(List())) ) )

    val ionDone = onDone[Int, Trampoline, Long](List(counter, inThrees))

    def isDone( i : Int, res : ResumableIterList[Int,Trampoline,Long]) =
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

    val starter = (ionDone &= iteratorEnumerator(liter)).eval

    val p =
      for {
        _ <- isDone(1, starter)

        // check it does not blow up the stack and/or mem
        r <- (foldM[Int, Trampoline, ResumableIterList[Int,Trampoline,Long]](starter){ (itr, i) =>
          val nitr = (extractCont(itr) &= iteratorEnumerator(liter)).eval
          Monad[Trampoline].map(nitr.value){
            _ =>
              isDone(i, nitr)
              nitr
          }
        } &= iteratorEnumerator((2 to maxIterations iterator))) run

        res = (extractCont(r) &= iteratorEnumerator(liter)).eval

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

    type ITER = ResumableIter[Int, Trampoline, Option[Int]]

    val itrOdd : ITER  = find[Int, Trampoline]( (x : Int) => x % 2 == 1 )

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
