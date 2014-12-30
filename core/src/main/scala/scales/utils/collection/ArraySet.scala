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
 * This mechanism exists to drop the outer pointer and class manifest instance.  Creates instances of ArraySet of varying size.
 */ 
trait ArraySetsFactory[A] {
  /**
   * @returns the equality Equal type class instance used
   */ 
  protected def equal: Equal[A]

  protected implicit def arrayManifest: ClassManifest[A]

  def emptySet: ArraySet[A]

  protected def one(a: A): ArraySet[A]

  protected def two(a: A, b: A): ArraySet[A]

  protected def three(a: A, b: A, c: A): ArraySet[A]

  protected def four(a: A, b: A, c: A, d: A): ArraySet[A]

  protected def five(a: A, b: A, c: A, d: A, e: A): ArraySet[A]

  protected def more(allTheAs: Array[A]): ArraySet[A]
}

object ArraySet {
  /**
   * Default implementation of the ArraySetFactoryFunctions.  Use to create a single instance.
   */ 
  def factory[A](implicit equalI: Equal[A], arrayManifestI: ClassManifest[A]): ArraySetsFactory[A] = {
    
    trait ASF extends ArraySetsFactory[A] {
      /**
       * @returns the equality Equal type class instance used
       */ 
      def equal: Equal[A] = equalI

      implicit def arrayManifest: ClassManifest[A] = arrayManifestI

      def emptySet: ArraySet[A] = new EmptyArraySet[A] with ASF

      def one(a: A): ArraySet[A] = new ArraySetOne[A] with ASF {
	val one = a
      }

      def two(a: A, b: A): ArraySet[A] = new ArraySetTwo[A] with ASF {
	val one = a
	val two = b
      }

      def three(a: A, b: A, c: A): ArraySet[A] = new ArraySetThree[A] with ASF {
	val one = a
	val two = b
	val three = c
      }

      def four(a: A, b: A, c: A, d: A): ArraySet[A] = new ArraySetFour[A] with ASF {
	val one = a
	val two = b
	val three = c
	val four = d
      }

      def five(a: A, b: A, c: A, d: A, e: A): ArraySet[A] = new ArraySetFive[A] with ASF {
	val one = a
	val two = b
	val three = c
	val four = d
	val five = e
      }

      def more(allTheAs: Array[A]): ArraySet[A] = new ArraySetArray[A] with ASF {
	val ar = allTheAs
      }
    }
    class ASFI extends ASF
    new ASFI()
  }
}

/**
 * ArraySet is an array backed set, with both the space savings and hit on performance.
 * The data is compared on the basis of the Equal type class instance provided only (no hashCodes etc). 
 */ 
trait ArraySet[A] extends Iterable[A] {

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

  def size: Int
}
