package scales.utils.collection.array

import scales.utils.collection.{ImmutableArrayProxy, ImmutableArrayProxyBuilder}

import collection.mutable.Builder
import scala.reflect.ClassManifest

object ImmutableArray {
  val emptyImmutableArray = new ImmutableArray[Nothing](Array[AnyRef](),0,0)
}

/**
 * Behaves like an ArrayList/ArrayBuffer, growing an internal array as necessary
 */
final class ImmutableArrayBuilder[ A ](private[this] var _buf : Array[AnyRef] = Array.ofDim[AnyRef](8)) extends Builder[A, ImmutableArray[A]]{

  final private[this] val gf : Float = 2.0f
  final private[this] val gp : Float  = 0.95f

  def resize( orig : Array[AnyRef], newCapacity : Int, len : Int ) = { 
    val ar = Array.ofDim[AnyRef](newCapacity)
    if (len != 0)
      Array.copy(orig, 0, ar, 0, len)

    ar
  }

  @inline def buf = _buf

  private[this] var _len = 0
  @inline def len = _len

  protected def ensureSize( size : Int ) {
    import java.lang.Math.round
    
    if ((_buf eq null) || (size > _buf.length))
      _buf = resize( _buf, size, _len )
    else if (size > round(_buf.length * gp)) {
      _buf = resize( _buf, round(_buf.length.toFloat * gf), _len )
    }
  }

  override def sizeHint( size : Int ) {
    ensureSize(size)
  }

  def result : ImmutableArray[A] = 
    if (_len == 0)
      ImmutableArray.emptyImmutableArray.asInstanceOf[ImmutableArray[A]]
    else
      ImmutableArray(_buf, 0, _len)

  //override def ++=(xs: TraversableOnce[A]): this.type = xs match {
  override def addAll(xs: IterableOnce[A]): ImmutableArrayBuilder.this.type = xs match {
    case ImmutableArray( base, offset, slen) =>
      ensureSize(_len + slen)
      Array.copy(base, offset, _buf, _len, slen)
      _len += slen
      this
    case _ =>
      var guessSize = {
        val k = xs.knownSize
        if (k == -1)
          5 // guess
        else
          k
      }
      ensureSize(_len + guessSize)
      var i = 0
      val itr = xs.iterator
      while(itr.hasNext) {
        val n = itr.next()
        if (_buf.length == _len + i) {
          // guess or provided size is bad
          guessSize += 5
          val b = _len
          _len += i
          ensureSize(guessSize)
          _len = b
        }
        _buf.update(_len + i, n.asInstanceOf[AnyRef])
        i+=1
      }

      _len += i
      this
  }

  override def addOne( elem : A) : this.type = {
  //def +=( elem : A) : this.type = {
    ensureSize(_len + 1)
    // we know its big enough
    _buf(_len) = elem.asInstanceOf[AnyRef]
    _len += 1
    this
  }

  def clear() {
    _len = 0
  }
}

case class IAEmpty[ +A ]() extends ImmutableArrayProxy[A] {
  def length = 0

  def apply(idx : Int) = scales.utils.error("Can't return an item, as we are empty")

  def ar = this  

  override def appended[B >: A](elem: B): ImmutableArrayProxy[B] =
    IAOne(elem)
}

case class IAOne[ +A ]( one : A ) extends ImmutableArrayProxy[A] {

  def apply(idx : Int) = one

  def length = 1

  def ar = this

  override def appended[B >: A](elem: B): ImmutableArrayProxy[B] =
    IATwo(one, elem)

  override def updated[B >: A](index: Int, elem: B): ImmutableArrayProxy[B] =
    IAOne(elem)
}

import scala.annotation.switch

case class IATwo[ +A ]( one : A, two : A ) extends ImmutableArrayProxy[A] {

  def apply(idx : Int) = (idx : @switch) match {
    case 0 => one
    case 1 => two
  }

  def length = 2

  def ar = this

  override def appended[B >: A](elem: B): ImmutableArrayProxy[B] =
    IAThree(one, two, elem)

  override def updated[B >: A](index: Int, elem: B): ImmutableArrayProxy[B] =
    ((index: @switch) match {
      case 0 => IATwo(elem, two)
      case 1 => IATwo(one, elem)
    })

}

case class IAThree[ +A ]( one : A, two : A, three : A ) extends ImmutableArrayProxy[A] {

  def apply(idx : Int) = (idx : @switch) match {
    case 0 => one
    case 1 => two
    case 2 => three
  }

  def length = 3

  def ar = this

  override def updated[B >: A](index: Int, elem: B): ImmutableArrayProxy[B] =
    ((index: @switch) match {
      case 0 => IAThree(elem, two, three)
      case 1 => IAThree(one, elem, three)
      case 2 => IAThree(one, two, elem)
    })

}


/**
 * Object arrays are just faster, System.arraycopy doesn't trust you and will type check everything, we can let nsc do that job for us.
 *
 * Same as ImmutableArray but for when the base is the entire collection, no offset or len are then needed
 */ 
trait ImmutableArrayT[ +A ] extends ImmutableArrayProxy[A] {
  val base : Array[AnyRef]
  def offset : Int
  def len : Int

  import ImmutableArrayProxyBuilder._

  def apply(idx : Int ) = base(idx + offset).asInstanceOf[A]
 
  def length = len

  override def prepended[B >: A](elem: B): ImmutableArrayProxy[B] =
  //@inline override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That =
    (if (len == vectorAfter)
      super.prepended(elem).asInstanceOf[ImmutableArrayProxy[B]]
    else {
      val ar = Array.ofDim[AnyRef](len+1)
      Array.copy(base, offset, ar, 1, len)
      ar(0) = elem.asInstanceOf[AnyRef]
      ImmutableArrayAll[B](ar)
    })

  override def appended[B >: A](elem: B): ImmutableArrayProxy[B] =
  //@inline override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That =
    (if (len == vectorAfter)
      super.appended(elem).asInstanceOf[ImmutableArrayProxy[B]]
    else {
      val ar = Array.ofDim[AnyRef](len+1)
      Array.copy(base, offset, ar, 0, len)
      ar(len) = elem.asInstanceOf[AnyRef]
      ImmutableArrayAll[B](ar)
    })//.asInstanceOf[That]

  @inline override def take( n : Int ) = 
    ImmutableArray(base, offset, if (len - n < 0) len else n)

  @inline override def drop( n : Int ) = 
    ImmutableArray(base, offset + n, if (len - n < 0) 0 else (len - n))

  @inline override def tail = drop(1)

  /**
   * we can do better - slice used by many functions in Optimized
   */ 
  @inline override def slice(from: Int, until: Int) = {
    val lo    = math.max(from, 0)
    val hi    = math.min(until, len)
    val elems = math.max(hi - lo, 0)
    ImmutableArray(base, offset, elems)
  }

  /**
   * Basically optimised version for back, hint used directly, one new array creation
   */
  override def updated[B >: A](index: Int, elem: B): ImmutableArrayT[B] = {
    // we know its objects underneath, we know the relationship is sound
    val ar = Array.ofDim[AnyRef](len)
    Array.copy(base, offset, ar, 0, len)
    ar(index) = elem.asInstanceOf[AnyRef]
    ImmutableArrayAll[B](ar)
  }

  /*override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That =
    if (bf.isInstanceOf[ImmutableArrayProxy.ImmutableArrayProxyCBF[_]]) {
      // we know its objects underneath, we know the relationship is sound
      val ar = Array.ofDim[AnyRef](len)
      Array.copy(base, offset, ar, 0, len)
      ar(index) = elem.asInstanceOf[AnyRef]
      ImmutableArrayAll[B](ar).asInstanceOf[That]
    } else {
      val b = bf(repr.asInstanceOf[ImmutableArrayProxy[A]])// we know it is as coll is this
      val (prefix, rest) = this.splitAt(index)
      b.sizeHint(len)
      b ++= prefix
      b += elem
      b ++= rest.tail
      b.result()
    }*/

  override def toArray[U >: A : ClassManifest]: Array[U] =
    if (implicitly[ClassManifest[U]].erasure eq base.getClass.getComponentType) {
      if ((offset == 0) && (len == base.length))
	base.asInstanceOf[Array[U]]
      else {
	val ar = Array.ofDim[U](len)
	Array.copy(base, offset, ar, 0, len)
	ar	
      }
    } else 
      super.toArray[U]


  def ar = this
}

/**
 * Don't add the offset and length, for building a dom this save 8 per elem, only matters for large docs (can save 4mb from 54mb), but can't hurt small ones.
 */ 
case class ImmutableArrayAll[ +A ]( base : Array[AnyRef]) extends ImmutableArrayT[A] {
  @inline final def offset = 0
  @inline final def len = base.length
  @inline final override def length = base.length
}

/**
 * Object arrays are just faster, System.arraycopy doesn't trust you and will type check everything, we can let nsc do that job for us.
 */ 
case class ImmutableArray[ +A ]( base : Array[AnyRef], offset : Int, len : Int) extends ImmutableArrayT[A]

/**
 * Proxy Vector.  When its in Vector it stays in Vector. 
 */ 
case class VectorImpl[ +A ](ar : Vector[A]) extends ImmutableArrayProxy[A] {

  def apply(idx : Int ) = ar.apply(idx)
 
  def length = ar.length

  override def prepended[B >: A](elem: B): VectorImpl[B] = VectorImpl(ar.+:(elem))

  override def appended[B >: A](elem: B): VectorImpl[B] = VectorImpl(ar.:+(elem))

  @inline override def take( n : Int ) = VectorImpl(ar.take(n))

  @inline override def drop( n : Int ) = VectorImpl(ar.drop(n))

  @inline override def tail = VectorImpl(ar.tail)

  @inline override def slice(from: Int, until: Int) = VectorImpl(ar.slice(from, until))

  override def updated[B >: A](index: Int, elem: B): VectorImpl[B] = VectorImpl(ar.updated(index, elem))
  //override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That = VectorImpl(ar.updated(index, elem)).asInstanceOf[That]

  override def toArray[U >: A : ClassManifest]: Array[U] = ar.toArray

}

