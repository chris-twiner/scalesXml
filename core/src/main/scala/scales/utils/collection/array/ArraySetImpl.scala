package scales.utils.collection

import annotation.tailrec

import scales.utils.Equiv

import scalaz._
import Scalaz._

import scales.utils.collection.array._

trait EmptyArraySet[A] extends ArraySet[A] with ArraySetsFactory[A] {
  
  def empty = true
  override def size = 0
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = false
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = None

  def + (elem: A): ArraySet[A] = one(elem)//new ArraySetOne(elem)
  def - (elem: A): ArraySet[A] = this

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = this

  def iterator = Iterator.empty.asInstanceOf[Iterator[A]]
}

trait ArraySetOne[A] extends EmptyArraySet[A] {
  val one: A

  protected def thisElem = one
  
  /**
   * used for iterator
   */ 
  protected def ups: List[A] = List(one)

  override def iterator = ups.iterator

  final override def empty = false
  override def size = minusOps.size
  override def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = equiv(b, thisElem) || super.contains(b)
  override def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = if (equiv(b, thisElem)) Some(thisElem) else super.apply(b)

  override def + (elem: A): ArraySet[A] = {
    val addp = add
    findMap[A, ArraySet[A]](
      addp._1)(
	addp._2(elem) // new arrayset instance
      )(equal.equal(_,_))(elem)
  }
//    if (equal.equal(one, elem)) one(elem)
//    else two(one, elem)

  protected def add: (List[(A, A => ArraySet[A])], A => ArraySet[A]) =
    (List(
      (one, i => one(i))
      ), a => two(one, a))

  def findMap[I,R](actions: List[(A, I => R)])(fallback: => R)(pred: (A, I) => Boolean)(in: I) : R =
    actions.find( a => pred(a._1, in)). // existing element
      map{ a => println("got one?");a._2(in) }.getOrElse{println("fallback");fallback} // call the action or return this 

  protected def minusOps: List[(A, Any => ArraySet[A])] = 
    List(
	(one, _ => emptySet)
      )

  protected def minusOp[I] = 
    findMap[I,ArraySet[A]](minusOps)(
	this
      ) _
   
  override def - (elem: A): ArraySet[A] = 
    minusOp[A](equal.equal(_,_))(elem)      

  override def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = 
    minusOp[B](equiv(_,_))(b)

}

trait ArraySetTwo[A] extends ArraySetOne[A] {
  val two: A

  override def thisElem = two  
  override protected def ups: List[A] = two :: super.ups

  override protected def add: (List[(A, A => ArraySet[A])], A => ArraySet[A]) = {
    println("Two add was called")
    (List(
	(two, i => two(one, i)),
	(one, i => two(i, two))
      ), a => {println("two fallback called");three(one, two, a)})
  }

  override def minusOps = 
    List(
	(two, _ => one(one)),
	(one, _ => one(two))
      )

}

trait ArraySetThree[A] extends ArraySetTwo[A] {
  val three: A

  override def thisElem = three
  override protected def ups: List[A] = three :: super.ups

  override protected def add: (List[(A, A => ArraySet[A])], A => ArraySet[A]) =
    (List(
	(three, i => three(one, two, i)),
	(two, i => three(one, i, three)),
	(one, i => three(i, two, three))
      ), a => four(one, two, three, a))

  override def minusOps = 
    List(
	(three, _ => two(one, two)),
	(two, _ => two(one, three)),
	(one, _ => two(two, three))
      )

}

trait ArraySetFour[A] extends ArraySetThree[A] {
  val four: A

  override def thisElem = four
  override protected def ups: List[A] = four :: super.ups

  override protected def add: (List[(A, A => ArraySet[A])], A => ArraySet[A]) =
    (List(
	(four, i => four(one, two, three, i)),
	(three, i => four(one, two, i, four)),
	(two, i => four(one, i, three, four)),
	(one, i => four(i, two, three, four))
      ), a => five(one, two, three, four, a))

  override def minusOps = 
    List(
	(four, _ => three(one, two, three)),
	(three, _ => three(one, two, four)),
	(two, _ => three(one, three, four)),
	(one, _ => three(two, three, four))
      )

}

trait ArraySetFive[A] extends ArraySetFour[A] {
  val five: A

  override def thisElem = five
  override protected def ups: List[A] = five :: super.ups

  override protected def add: (List[(A, A => ArraySet[A])], A => ArraySet[A]) = {
    val nullA = null.asInstanceOf[A]
    (List(
	(five, i => five(one, two, three, four, i)),
	(four, i => five(one, two, three, i, five)),
	(three, i => five(one, two, i, four, five)),
	(two, i => five(one, i, three, four, five)),
	(one, i => five(i, two, three, four, five))
      ), a => more(Array(one, two, three, four, five, a
			 ))) // give max in same alignment
  }

  override def minusOps = 
    List(
	(five, _ => four(one, two, three, four)),
	(four, _ => four(one, two, three, five)),
	(three, _ => four(one, two, four, five)),
	(two, _ => four(one, three, four, five)),
	(one, _ => four(two, three, four, five))
      )

}

/**
 * Exact array only, grows by one / shrinks by one
 */ 
trait ArraySetArray[A] extends ArraySet[A] with ArraySetsFactory[A] {
  
  val ar: Array[A]

  def iterator = ar.take(size).iterator

  override def size = ar.length

  def empty = size != 0
  // impl lets it grow def size = ar.length
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = indexOf(b)(equiv(_,_)) != -1
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = {
    val i = indexOf(b)(equiv(_,_))
    if (i == -1)
      None
    else
      Some(ar(i))
  }

  protected def indexOf[I](in: I)(pred: (A, I) => Boolean): Int = {
    var i = 0
    var found = false
    val sized = size
    while( i < sized && !found ){
      if (pred(ar(i), in))
	found = true
      else 
	i += 1
    }
    if (found)
      i
    else
      -1
  }

  protected def newArray(growBy: Int, copy: Boolean = false) = {
    val na = Array.ofDim[A](
      (ar.length + growBy).toInt
    )
    if (copy) {
      Array.copy(ar, 0, na, 0, size)
    }
    na
  }

  def + (elem: A): ArraySet[A] = {
    val mi = indexOf(elem)(equal.equal(_,_))

    val newarray = newArray(
      if (mi == -1) 1
      else 0
      , true)
    newarray.update(
      if (mi == -1) 
	size
      else
	mi, elem)
    more(newarray)
  }

  protected def minusOp[I](pred: (I, A) => Boolean, elem: I) = {
    val mi = indexOf(elem)((a, i) => pred(i, a))

    if (mi == -1)
      this
    else {
      if (size == 6)
	mi match {
	  case 0 => five(ar(1), ar(2), ar(3), ar(4), ar(5))
	  case 1 => five(ar(0), ar(2), ar(3), ar(4), ar(5))
	  case 2 => five(ar(0), ar(1), ar(3), ar(4), ar(5))
	  case 3 => five(ar(0), ar(1), ar(2), ar(4), ar(5))
	  case 4 => five(ar(0), ar(1), ar(2), ar(3), ar(5))
	  case 5 => five(ar(0), ar(1), ar(2), ar(3), ar(4))
	}
      else {
	val na = newArray(-1)
	Array.copy(ar, 0, na, 0, mi)
	Array.copy(ar, mi + 1, na, mi, na.length - mi)
	more(na)
      }
    }
  }

  def - (elem: A): ArraySet[A] =
    minusOp[A](equal.equal(_,_), elem)

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = 
    minusOp[C](equiv(_,_), b)

}

/*

/**
 * Grows by 0.5 when needed, and shrinks back to one -> five where possible.
 * Instances must override size
 */ 
trait ArraySetArray[A] extends ArraySet[A] with ArraySetsFactory[A] {
  
  val ar: Array[A]

  def iterator = ar.take(size).iterator

  def empty = size != 0
  // impl lets it grow def size = ar.length
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = indexOf(b)(equiv(_,_)) != -1
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = {
    val i = indexOf(b)(equiv(_,_))
    if (i == -1)
      None
    else
      Some(ar(i))
  }

  protected def indexOf[I](in: I)(pred: (A, I) => Boolean): Int = {
    var i = 0
    var found = false
    val sized = size
    while( i < sized && !found ){
      if (pred(ar(i), in))
	found = true
      else 
	i += 1
    }
    if (found)
      i
    else
      -1
  }

  protected def newArray(grow: Boolean) = {
    val na = Array.ofDim[A](
      (if (grow) 
	ar.length * 0.5
      else
	ar.length).toInt
    )
    Array.copy(ar, 0, na, 0, size)
    na
  }

  def + (elem: A): ArraySet[A] = {
    val mi = indexOf(elem)(equal.equal(_,_))

    if (mi == -1) {
      // grow by 0.5 and add at the end if no space
      val newarray = 
	if (mi == (ar.length -1))
	  newArray(true) // grow on add 
	else
	  ar.clone
      newarray.update(size, elem)
      more(newarray, size + 1)
    } else {
      // copy array replace at mi
      val na = newArray(false)
      na.update(mi, elem)
      more(na, size)
    }
  }

  protected def minusOp[I](pred: (I, A) => Boolean, elem: I) = {
    val mi = indexOf(elem)((a, i) => pred(i, a))

    if (mi == -1)
      this
    else {
      if (size == 6)
	mi match {
	  case 0 => five(ar(1), ar(2), ar(3), ar(4), ar(5))
	  case 1 => five(ar(0), ar(2), ar(3), ar(4), ar(5))
	  case 2 => five(ar(0), ar(1), ar(3), ar(4), ar(5))
	  case 3 => five(ar(0), ar(1), ar(2), ar(4), ar(5))
	  case 4 => five(ar(0), ar(1), ar(2), ar(3), ar(5))
	  case 5 => five(ar(0), ar(1), ar(2), ar(3), ar(4))
	}
      else {
	val na = ar.clone
	Array.copy(ar, mi + 1, na, mi, size - mi -1)
	more(na, size -1)
      }
    }
  }

  def - (elem: A): ArraySet[A] =
    minusOp[A](equal.equal(_,_), elem)

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = 
    minusOp[C](equiv(_,_), b)

}


*/
