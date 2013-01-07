package scales.utils
import ScalesUtils._

class IterateeTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._

  import scalaz._
  import Scalaz._
  import scalaz.IterV._
  
  def testSimpleEnumerateeMap = {
    
    val i = List("1","2","3","4").iterator

    val res = enumerateeMap(sum[Int])( (s : String) => s.toInt )(i).run
    assertEquals(10, res)
  }
  
  def testEnumToManyExhaustState = {
    
    val l = List(1,2,3,4)

    def f(i: Int): ResumableIter[Int, EphemeralStream[Int]] = {
      def step(i: Int)(s: Input[Int]): ResumableIter[Int, EphemeralStream[Int]] = 
	s( el = e => {
	    //println("i "+i+" e "+e)
	    Done((iTo(i, e), Cont(step(i + 1))), IterV.Empty[Int])
	  },
	  empty = Cont(step(i)),
	  eof = Done((EphemeralStream.empty, Cont(error("Shouldn't call cont on eof"))), IterV.EOF[Int])
	)

      Cont(step(i))
    }

    val enum = (i: Int) => enumToMany(sum[Int])(f(i))

    // 1, 2, 3, 4 only, the to is ignored
    val (res, cont) = enum(1)(l.iterator).run
    assertEquals(10, res)
    assertTrue("should have been done", isDone(cont))

    // -1->1 0, 0->2 3, 1->3 6, 2->4 9 
    val (res2, cont2) = enum(-1)(l.iterator).run
    assertEquals(18, res2)
    assertTrue("2 should have been done", isDone(cont2))

  }

  def testEnumToManyExhaust = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = enumToMany(sum[Int])( mapTo( (i: Int) => El(iTo(1, i)) ) )(i).run
    assertEquals(20, res)
    assertTrue("should have been done", isDone(cont))
  }

  def testEnumToManyEOFFromMap = {
    
    val i = List(1,2,3,4).iterator

    val (res, cont) = enumToMany(sum[Int])( mapTo( (i: Int) => EOF[EphemeralStream[Int]] ) )(i).run
    assertEquals(0, res)
    assertTrue("should have been done", isDone(cont))

    val rest = i.take(4).toSeq
    //println(rest)
    assertEquals("Should still have had items in the list", 9, rest.sum)
  }

  /**
   * Make sure it doesn't soe in the loop
   */ 
  def testEnumToManySOE = {
    val i = List[Long](1,2,3,4,5).iterator
    
    val (res, cont) = enumToMany(sum[Long])( mapTo( (_:Long) => El(lTo(1L, 100000L)) ) )(i).run
    assertEquals(25000250000L, res)
    assertTrue("should have been done", isDone(cont))
  }

  def lTo(lower: Long, upper: Long): EphemeralStream[Long] =
    if (lower > upper) EphemeralStream.empty else EphemeralStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) EphemeralStream.empty else EphemeralStream.cons(lower, iTo(lower + 1, upper))

  def testEventsNotLostOneToMany = {
    val i = List[Long](1,2,3,4,5).iterator

    // force a restart at magic number 1
    val sum: ResumableIter[Long,Long] = {
      def step(acc: Long)( s : Input[Long] ) : ResumableIter[Long, Long] =
	s( el = e => {
	    val nacc = acc + e
	    if (nacc == 25000050001L)
	      Done((nacc, Cont(step(nacc))), IterV.Empty[Long])
	    else
	      Cont(step(nacc))
	  },
	  empty = Cont(step(acc)),
	  eof = Done((acc, Done(acc, IterV.EOF[Long])), IterV.EOF[Long])
	)

      Cont(step(0))
    }

    val (res, cont) = enumToMany[Long, Long, Long](sum)( mapTo( (_:Long) => El(lTo(1L, 100000L)) ) )(i).run
    assertEquals(25000050001L, res)
    assertTrue("should not have been done", !isDone(cont))

    val (res2, cont2) = (cont.asInstanceOf[ResumableIter[Long, Long]]).run
    assertEquals(25000250000L, res2)
    assertTrue("should have been done", isDone(cont2))
  }

}
