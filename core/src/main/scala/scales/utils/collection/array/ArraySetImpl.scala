package scales.utils.collection

import annotation.tailrec

import scales.utils.Equiv

import scalaz._
import Scalaz._

import scales.utils.collection.array._

trait EmptyArraySet[A] extends ArraySet[A] with ArraySetFactoryFunctions[A] {
  
  def empty = true
  def size = 0
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = false
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = None

  def + (elem: A): ArraySet[A] = one(elem)//new ArraySetOne(elem)
  def - (elem: A): ArraySet[A] = this

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = this

}

trait ArraySetOne[A] extends EmptyArraySet[A] {
  val one: A

  protected def thisElem = one
  
  final override def empty = false
  override def size = 1
  override def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean = equiv(b, thisElem) || super.contains(b)
  override def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A] = if (equiv(b, thisElem)) Some(thisElem) else super.apply(b)

  def + (elem: A): ArraySet[A] = 
    if (equal.equal(one, elem)) one(elem)
    else two(one, elem)

  def - (elem: A): ArraySet[A] = 
    if (equal.equal(one, elem)) emptySet
    else this

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = 
    if (equiv(b, one)) emptySet 
    else this
}


trait ArraySetTwo[A] extends ArraySetOne[A] {
  val two: A

  override def thisElem = two  
  override def size = 2

  def findMap[I,R](actions: List[(A, I => R)])(fallback: => R)(pred: (A, I) => Boolean)(in: I) : R =
    actions.find( a => pred(a._1, in)). // existing element
      map( a => a._2(in)).getOrElse(fallback) // call the action or return this

  override def + (elem: A): ArraySet[A] = 
    findMap[A, ArraySet[A]](
      List(
	(two, i => two(one, elem)),
	(one, i => two(elem, two))
	))(
	this  // new three
      )(equal.equal(_,_))(elem)

  def minusOp[I] = 
    findMap[I,ArraySet[A]](List(
	(two, _ => one(one)),
	(one, _ => one(two))
      ))(
	this
      ) _
   
  override def - (elem: A): ArraySet[A] = 
    minusOp[A](equal.equal(_,_))(elem)      

  override def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A] = 
    minusOp[B](equiv(_,_))(b)
}

