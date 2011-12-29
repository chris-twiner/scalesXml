package scales.utils

import scala.collection._
import immutable.{Set}

import scala.math.Ordering
import generic._
import mutable.{ Builder, SetBuilder }

/**
 * Place Holder for equals and hashcode
 */ 
trait Key[A]

/** $factoryInfo
 *  @define Coll immutable.TreeSet
 *  @define coll immutable tree set
 */
object MapSet {
  def newBuilder[A](implicit conv : A => Key[A]) : Builder[A, MapSet[A]] =
    new SetBuilder(empty[A])

  /** The empty set of this type
   */
  def empty[A](implicit conv : A => Key[A]) = new MapSet[A]()(conv)

  class DirectKey[A](val a : A) extends Key[A] {
    override def equals( other : Any ) = other match {
      case d : DirectKey[A] => a == d.a
      case _ => false
    }
    override def hashCode() = a.hashCode
  }

  implicit def directKey[A]( a : A ) : Key[A] = new DirectKey(a)
}

/**
 * This version of treeset better fits the xml attributes, we also want to be able to get stuff out.  So its A -> A but hidden in a set
 */
@serializable 
@SerialVersionUID(-234066569778569402L)
class MapSet[A](val map : Map[Key[A],A])
  (implicit val conv : A => Key[A])
  extends Set[A] with SetLike[A, MapSet[A]] {

  override def stringPrefix = "MapSet"

  def this()(implicit conv : A => Key[A]) = this(Map[Key[A],A]())(conv)
  
  private def newSet(m : Map[Key[A],A]) = new MapSet[A](m)(conv)

  override def newBuilder : Builder[A, MapSet[A]] = MapSet.newBuilder

  /** A factory to create empty sets of the same type of keys.
   */
  override def empty = MapSet.empty

  override val size = map.size

  /**
   * Look ma now we are a map CTw
   */
  def apply[T](a : T)(implicit conv : (T) => Key[A]) : Option[A] =
    map.get(conv(a))

  /**
   * Convertable contains
   */
  def contains[T](elem: T)(implicit conv : (T) => Key[A] ): Boolean = 
    map.contains(conv(elem))

  /**
   * Convertable --
   */ 
  def --[T]( other : Traversable[T] )(implicit conv : (T) => Key[A]) = other.foldLeft(this)(_ - _)

  /**
   * Convertable -
   */
  def -[T]( other : T) (implicit conv : (T) => Key[A]) = {
    val nm = map - conv(other)
    if (nm eq map) this
    else newSet(nm)
  }

  /** Creates a new `TreeSet` with the entry added.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def + (elem: A): MapSet[A] = {
    val m = map.updated(conv(elem), elem)
    if (m eq map) this // highly unlikely though
    else newSet(m)
  }

  /** A new `TreeSet` with the entry added is returned,
   *  assuming that elem is <em>not</em> in the TreeSet.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing `elem` and all the elements of this $coll.
   */
  def insert(elem: A): MapSet[A] = {
    val key = conv(elem)
    assert(map.get(key).isEmpty)
    newSet(map.updated(key, elem))
  }
  
  /** Creates a new `TreeSet` with the entry removed.
   *  
   *  @param elem    a new element to add.
   *  @return        a new $coll containing all the elements of this $coll except `elem`.
   */
  def - (elem:A): MapSet[A] = {
    val m = map - conv(elem)
    if (map eq m) this
    else newSet(m)
  }
  /** Checks if this set contains element `elem`.
   *  
   *  @param  elem    the element to check for membership.
   *  @return true, iff `elem` is contained in this set.
   */
  def contains(elem: A): Boolean = map.contains(conv(elem))

  /** Creates a new iterator over all elements contained in this
   *  object.
   *  
   *  @return the new iterator
   */
  def iterator: Iterator[A] = map.valuesIterator

  override def toStream: Stream[A] = map.values.toStream

// for some reason the below is whats called but not seen
//  override def foreach[U](f: A => U) = iterator foreach f

  override def equals( other : Any ) = other match {
    case o : MapSet[A] => o.map == map // map should be faster directly
    case _ => false
  }
}
