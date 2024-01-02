package scales.utils
import ScalesUtils._
import scalaz.Free.Trampoline
import scalaz.Id.Id
import scalaz.iteratee.EnumeratorT
import scalaz.iteratee.Input.{Element, Empty, Eof}
import scalaz.iteratee.Iteratee.{iterateeT => siteratee}
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.Free._
import scalaz.Monad

import scalaz.Scalaz._

class IterateeTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._

  import scalaz.iteratee.{Iteratee, Enumerator, Input}
  import scalaz.EphemeralStream
  import EphemeralStream.emptyEphemeralStream

  import scales.utils.{sum => usum}
  import scalaz.effect.IO._

  import scalaz.Scalaz._

  def F(implicit F: Monad[Trampoline]) = F

  def testSimpleEnumerateeMap(): Unit = {
    
    val i = List("1","2","3","4").iterator

    (enumerateeMap(usum[Int, Trampoline])( (s : String) => s.toInt ) &= iteratorEnumerator(i) ).run map
         { res =>
      assertEquals(10, res)
    } run
  }
  /*
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
            eof = Done((emptyEphemeralStream, siteratee( F.point(  Cont(error("Shouldn't call cont on eof")) ))), Eof[Int])
          ))
        )

      siteratee( F.point( Cont(step(i)) ))
    }

    val enum = (i: Int) => enumToMany(usum[Int, Trampoline])(f(i))

    for {
      // 1, 2, 3, 4 only, the to is ignored
      (res, cont) <- (enum(1) &= iteratorEnumerator(l.iterator)).run
      is <- isDone(cont)
    } yield {
      assertEquals(10, res)

      assertTrue("should have been done", is)
    }

    for {
      // -1->1 0, 0->2 3, 1->3 6, 2->4 9
      (res2, cont2) <- (enum(-1) &= (iteratorEnumerator(l.iterator))).run
      is <- isDone(cont2)
    } yield {
      assertEquals(18, res2)

      assertTrue("2 should have been done", is)
    }
  }

  def testEnumToManyExhaust = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = (enumToMany(usum[Int])( mapTo( (i: Int) => Element(iTo(1, i)) ) ) &= iteratorEnumerator(i)).run
    assertEquals(20, res)
    assertTrue("should have been done", isDone(cont))
  }
*/
  def testEnumToManyEOFFromMap = {
    
    val i = List(1,2,3,4).iterator

    val p =
      for {
        r <- (enumToMany(usum[Int, Trampoline])( mapTo[Int, Trampoline, Int]( (i: Int) => Eof[EphemeralStream[Int]] ) ) &= iteratorEnumerator(i)).run
        (res, cont) = r
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
        (res, cont) = r
        done <- isDone(cont)
      } yield {
        assertEquals (25000250000L, res)
        assertTrue ("should have been done", done)
      }

    p.run
  }

  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) emptyEphemeralStream else EphemeralStream.cons(lower, iTo(lower + 1, upper))

/*
  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // force a restart at magic number 1
    val sum: ResumableIter[Long,Long] = {
      def step(acc: Long)( s : Input[Long] ) : ResumableIter[Long, Long] =
        siteratee(
          s( el = e => {
              val nacc = acc + e
              if (nacc == 25000050001L)
                Done((nacc, siteratee( Cont(step(nacc))) ), Empty[Long])
              else
                Cont(step(nacc))
            },
            empty = Cont(step(acc)),
            eof = Done((acc, siteratee( Done(acc, Eof[Long]))), Eof[Long])
          )
        )

      siteratee( Cont(step(0)))
    }

    val (res, cont) = (enumToMany[Long, Long, Long](sum)( mapTo( (_:Long) => Element(lTo(1L, 100000L)) ) ) &= iteratorEnumerator(i)).run
    assertEquals(25000050001L, res)
    assertFalse("should not have been done", isDone(cont))

    val (res2, cont2) = (cont.asInstanceOf[ResumableIter[Long, Long]]).run
    assertEquals(25000250000L, res2)
    assertTrue("should have been done", isDone(cont2))
  }

  def testEveryItem = {
    val source = List[Long](1,4,7,10,13).iterator

    // force a restart every entry
    val echo: ResumableIter[Long,Long] = {
      def step( s : Input[Long] ) : ResumableIter[Long, Long] =
        siteratee(
          s( el = e => {
        //	  println("got "+e)
              //if (e > 2) {
                Done((e, siteratee(Cont(step))), Empty[Long])
              //} else Cont(step)
              //
            },
            empty = Cont(step),
            eof = Done((0, siteratee(Done(0, Eof[Long]))), Eof[Long])
          )
        )

      siteratee( Cont(step) )
    }

    val (origres, origcont) = (enumToMany[Long, Long, Long](echo)( mapTo{
      (x:Long) =>
	//println("evaled to "+x);
        Element(lTo(x, x + 2L))
          }) &= (iteratorEnumerator(source))).run

    var res = origres
    var iter : ResumableIter[Long, Long] = origcont.asInstanceOf[ResumableIter[Long, Long]]

    var count = 1 // already run once

    for( i <- 1 to 15 ) {
//      println("loop !!!! "+i)
      assertEquals(i, res)
      val cres : ResumableIter[Long, Long] = iter.eval
      if (isDone(cres)){
        res = extract(cres).get
        iter = extractCont(cres)
      } else {
        //	println(">>><<<<>>>>><<<<")
        count += 1
        // run on the source - as we need more data
        val (nres, ncont) = (cres &= iteratorEnumerator(source)).run
        res = nres
        iter = ncont.asInstanceOf[ResumableIter[Long, Long]]

        if (count == 6) {
          assertEquals("should have pushed the eof", 15, i)
        }
      }
    }

    assertEquals(0, res)
    assertTrue("should have been done", isDone(iter))
    assertTrue("should have been eof", isEOF(iter))
    assertFalse("source should be empty", source.hasNext)
    assertEquals("should have reset 6 times - once for start and once for EOF ", 6, count)
  }
*/
}
