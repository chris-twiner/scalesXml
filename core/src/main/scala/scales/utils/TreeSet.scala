package scales.utils

import scala.collection._
import immutable.{SortedSet, RedBlack}

import scala.math.Ordering
import generic._
import mutable.{ Builder, SetBuilder }

/** $factoryInfo
 *  @define Coll immutable.TreeSet
 *  @define coll immutable tree set
 */
object TreeSet extends ImmutableSortedSetFactory[TreeSet] {
  implicit def implicitBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] = newBuilder[A](ordering)
  override def newBuilder[A](implicit ordering: Ordering[A]): Builder[A, TreeSet[A]] =
    new SetBuilder(empty[A](ordering))

  /** The empty set of this type
   */
  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]
}

/**
 * This version of treeset better fits the xml attributes, we also want to be able to get stuff out.  So its A -> A but hidden in a set
 */
@serializable
@SerialVersionUID(-234066569443569402L)
class TreeSet[A](override val size: Int, t: RedBlack[A]#Tree[A])
                (implicit val ordering: Ordering[A])
  extends RedBlack[A] with SortedSet[A] with SortedSetLike[A, TreeSet[A]] {

  override def stringPrefix = "TreeSet"

  def isSmaller(x: A, y: A) = compare(x,y) < 0

  def this()(implicit ordering: Ordering[A]) = this(0, null)(ordering)
  
  protected val tree: RedBlack[A]#Tree[A] = if (size == 0) Empty else t

  private def newSet(s: Int, t: RedBlack[A]#Tree[A]) = new TreeSet[A](s, t)

  override def newBuilder : Builder[A, TreeSet[A]] = TreeSet.newBuilder

  /** A factory to create empty sets of the same type of keys.
   */
  override def empty = TreeSet.empty

  /**
   * Look ma now we are a map CTw
   */
  def apply[T](a : T)(implicit conv : (T) => A) : Option[A] =
    tree.lookup(conv(a)) match {
      case n: NonEmpty[a] => Some(n.value)
      case _ => None
    }

  /**
   * Convertable contains
   */
  def contains[T](elem: T)(implicit conv : (T) => A ): Boolean = !tree.lookup(conv(elem)).isEmpty

  /**
   * Convertable --
   */ 
  def --[T]( other : Traversable[T] )(implicit conv : (T) => A) = other.foldLeft(this)(_ - _)

  /**
   * Convertable -
   */
  def -[T]( other : T) (implicit conv : (T) => A) = {
    val elem = conv(other)

    if (tree.lookup(elem).isEmpty) this
    else newSet(size - 1, tree delete elem)
  }

  /** Creates a new `TreeSet` with the entry added.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def + (elem: A): TreeSet[A] = {
    val newsize = if (tree.lookup(elem).isEmpty) size + 1 else size
    newSet(newsize, tree.update(elem, elem))
  }

  /** A new `TreeSet` with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def insert(elem: A): TreeSet[A] = {
    assert(tree.lookup(elem).isEmpty)
    newSet(size + 1, tree.update(elem, elem))
  }
  
  /** Creates a new `TreeSet` with the entry removed.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing all the elements of this $coll except `elem`.
   */
  def - (elem:A): TreeSet[A] = 
    if (tree.lookup(elem).isEmpty) this
    else newSet(size - 1, tree delete elem)

  /** Checks if this set contains element `elem`.
   *  
   *  @param  elem    the element to check for membership.
   *  @return true, iff `elem` is contained in this set.
   */
  def contains(elem: A): Boolean = !tree.lookup(elem).isEmpty

  /** Creates a new iterator over all elements contained in this
   *  object.
   *  
   *  @return the new iterator
   */
  def iterator: Iterator[A] = tree.toStream.iterator map (_._1)

  override def toStream: Stream[A] = tree.toStream map (_._1)

  override def foreach[U](f: A =>  U) = tree foreach { (x, y) => f(x) } 

  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = {
    val tree = this.tree.range(from, until)
    newSet(tree.count, tree)
  }
  override def firstKey = tree.first
  override def lastKey = tree.last
}
