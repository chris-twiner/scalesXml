package scales.utils.collection

import annotation.tailrec

import scales.utils.Equiv

import scalaz._
import Scalaz._

import scales.utils.collection.array._

/**
 * Create ArraySet instances
 * 
object ArraySet {
  def empty[A: Equal] = new EmptyArraySet[A]
  def apply[A: Equal]( a: A*) = empty[A] ++ (a)
}
*/

/**
 * This mechanism exists to drop the outer pointer, boilerplatey :<
 */ 
trait ArraySetFactoryFunctions[A] {
  /**
   * @returns the equality Equal type class instance used
   */ 
  def equal: Equal[A]

  def emptySet: ArraySet[A]

  def one(a: A): ArraySet[A]

  def two(a: A, b: A): ArraySet[A]
}

/**
 * ArraySet is an array backed set, with both the space savings and hit on performance.
 * The data is compared on the basis of the Equal type class instance provided only (no hashCodes etc). 
 */ 
trait ArraySet[A] {

  /**
   * Uses the Equal instance to add extra elements
   */ 
  def ++( other : Traversable[A] ): ArraySet[A] = other.foldLeft(this)(_ + _)

  /**
   * Uses the Equal instance to remove elements
   */ 
  def --( other : Traversable[A] ): ArraySet[A] = other.foldLeft(this)(_ - _)

  /**
   * Remove items from this set based on an equivalence relationship between the two types against a third type C.
   */ 
  def --[B,C]( other : Traversable[B] )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) = other.foldLeft(this)(_ - _)

  /**
   * With another level of indirection, the caller decides what Equiv to use.
   */ 
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Boolean

  /**
   * Access the set like a map with a given Equiv[C] instance and conversion functions to C
   */ 
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C): Option[A]

  /**
   * This method creates a new set with an additional element against the Equal instance.
   */
  def + (elem: A): ArraySet[A]
  
  /** <code>-</code> can be used to remove a single element from
   *  a set.
   */
  def - (elem: A): ArraySet[A]

  /**
   * Removes a single element via a defined Equiv relationship between the types
   */ 
  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ArraySet[A]

  def empty: Boolean

  def size: Integer
}
