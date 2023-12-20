package scales.utils.collection

import annotation.tailrec

import scales.utils.Equiv

import scalaz._
import Scalaz._

//TODO also integrate the non quadratic add/apply from PaulPs fixes, tough as we can't use hashCode or == directly we can't re-use hashset....

object ListSet {
  def empty[A : Equal] = new ListSet[A]
  def apply[A : Equal]( a : A*) = empty[A] ++ (a)
}

/** 
 *  Based on Scala ListSet, users provide the
 *  comparisom operator and comparisom type for lookups.
 *
 *  Because the equality is defined outside of the object + always acts as add/replace
 *  
 *  @tparam A    the type of the elements contained in this list set.
 *  
 *  @author Originally Matthias Zenger
 *  @author <- corrupted by Chris Twiner with a number of PaulPs fixes
 *  @version 1.0, 30/12/2010
 */
class ListSet[A : Equal](val plusFast : Boolean = false) extends Iterable[A] with Serializable { self =>

  val equal = implicitly[Equal[A]]

  def ++( other : Traversable[A] ) = other.foldLeft(this)(_ + _)

  def --( other : Traversable[A] ) = other.foldLeft(this)(_ - _)

  def --[B,C]( other : Traversable[B] )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) = other.foldLeft(this)(_ - _)

  /** Returns the number of elements in this set.
   *
   *  @return number of set elements.
   */
  override def size: Int = 0
  override def isEmpty: Boolean = true;

  /** Checks if this set contains element <code>elem</code>.
   *
   *  @param  elem    the element to check for membership.
   *  @return true, iff <code>elem</code> is contained in this set.
   */
  def contains(elem: A): Boolean = false

  /**
   * The default empty doesn't really work for us as we can't hide away the equality, so doing it here
  override def empty = new ListSet[A]
   */

  /**
   * With another level of indirection, the caller decides what Equal to use.
   */ 
  def contains[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) = false

  /**
   * Access the set like a map with an alternate equals and conversion function
   */ 
  def apply[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : Option[A] = None

  /** This method creates a new set with an additional element.
   */
  def + (elem: A): ListSet[A] = new Node(elem)
  
  /**
   * Does not remove any existing As first, hence not safe.
   */ 
  def unsafePlus(e: A): ListSet[A] = new Node(e)

  /** <code>-</code> can be used to remove a single element from
   *  a set.
   */
  def - (elem: A): ListSet[A] = this

  def -[B,C]( b : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ListSet[A] = this
	  
  private[ListSet] def unchecked_outer: ListSet[A] =
    throw new NoSuchElementException("Empty ListSet has no outer pointer")

  /** Creates a new iterator over all elements contained in this set.
   *
   *  @throws Predef.NoSuchElementException
   *  @return the new iterator
   */
  def iterator: Iterator[A] = new Iterator[A] {
    var that: ListSet[A] = self;
    def hasNext = !that.isEmpty;
    def next: A =
      if (!hasNext) throw new NoSuchElementException("next on empty iterator")
      else { val res = that.elem; that = that.next; res }
  }

  /**
   *  @throws Predef.NoSuchElementException
   */
  protected def elem: A = throw new NoSuchElementException("Set has no elements");

  /**
   *  @throws Predef.NoSuchElementException
   */
  protected def next: ListSet[A] = throw new NoSuchElementException("Next of an empty set");

  protected def newThis(a : A) = new Node(a)

  /** Represents an entry in the `ListSet`.
   */
  protected class Node(override protected val elem: A) extends ListSet[A] with Serializable {

    override private[ListSet] def unchecked_outer = self

    /** Returns the number of elements in this set.
     *
     *  @return number of set elements.
     */
    override def size = {
      @tailrec def sizeInternal(n: ListSet[A], acc: Int): Int =
	if (n.isEmpty) acc
	else sizeInternal(n.unchecked_outer, acc + 1)
      
      sizeInternal(this, 0)
    }
    /** Checks if this set is empty.
     *
     *  @return true, iff there is no element in the set.
     */
    override def isEmpty: Boolean = false
  
    /** Checks if this set contains element <code>elem</code>.
     *
     *  @param  elem    the element to check for membership.
     *  @return true, iff <code>elem</code> is contained in this set.
     */
    override def contains(e: A) = { 
      @tailrec def containsInternal(n: ListSet[A], e: A): Boolean = 
	!n.isEmpty && (equal.equal(e, n.elem) || containsInternal(n.unchecked_outer, e))

      containsInternal(this, e)
    }

    override def contains[B,C]( e : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) = {
      @tailrec def containsInternal(n: ListSet[A], e: B): Boolean = 
	!n.isEmpty && (
	  equiv(e, n.elem)
	  || containsInternal(n.unchecked_outer, e) )

      containsInternal(this, e)
    }

    override def apply[B,C]( e : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : Option[A] = {
      @tailrec def applyInternal(n : ListSet[A], e : B) : Option[A] =
	if (n.isEmpty)
	  None
        else
	  if (equiv(e, n.elem))
	    Some(n.elem)
	  else
	    applyInternal(n.unchecked_outer, e)

      applyInternal(this, e)
    }
  
    /** This method creates a new set with an additional element.
     * Note if plusFast is true then the 
     */
    override def +(e: A): ListSet[A] = if (contains(e)) this.-(e).newThis(e) else new Node(e)
        
    override def unsafePlus(e: A): ListSet[A] = new Node(e)

    /** <code>-</code> can be used to remove a single element from
     *  a set.
     */
    override def -(e: A): ListSet[A] = if (equal.equal(e, elem)) self else {
      val tail = self - e; new tail.Node(elem)
    }

    override def -[B,C]( e : B )(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) : ListSet[A] = 
      if (equiv(e, elem)) self else {
	val tail = self - e; new tail.Node(elem)
      }

    override protected def newThis(a : A) = new Node(a)

    override protected def next: ListSet[A] = self
    
  }
}
