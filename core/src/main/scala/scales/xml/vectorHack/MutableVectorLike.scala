
package scala.collection.immutable {

import scala.annotation.unchecked.uncheckedVariance

import scales.utils._

//import collection.{IndexedSeqOptimized, IndexedSeqLike, IndexedSeq}
import collection.mutable.Builder //{ArrayBuilder, Builder}
import collection.generic.{CanBuildFrom, GenericTraversableTemplate, SeqFactory, GenericCompanion}

/**
 * Don't use this unless you are not having your own OptimisationStrategies.
 *
 * Basic copy of a VectorBuilder but without result etc, this only uses ar
 * and returns either a smaller immutablearray or a vectorimpl.
 *
 * The whole idea is not to routinely copy the tree around and provide
 * very fast updates and access to the current head (i.e. zipUp)
 */ 
final class MutableVectorLike[+A](val copyArray : Boolean = false) extends ImmutableArrayProxy[A] with VectorPointer[A @uncheckedVariance]{

  override def companion: GenericCompanion[ImmutableArrayProxy] = err

  override protected[this] def newBuilder: Builder[A, ImmutableArrayProxy[A]] = err

  display0 = new Array[AnyRef](32)
  depth = 1

  def err = error("MutableVector like only supports apply(length-1), updated(length - 1), :+ and immutable")

  private var blockIndex = 0
  private var lo = 0

  private var len = 0

  def apply(idx : Int ) = err
/*    if (idx != (len -1))
      err
    else
      display0(lo-1).asInstanceOf[A]
*/
  @inline override def length = len
  
  @inline override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That = err
  
  @inline override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That = {
    if (lo >= display0.length) {
      val newBlockIndex = blockIndex+32
      gotoNextBlockStartWritable(newBlockIndex, blockIndex ^ newBlockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
    display0(lo) = elem.asInstanceOf[AnyRef]
    lo += 1
    len += 1
    this.asInstanceOf[That]
  }

  /**
   * Basically optimised version for back, hint used directly, one new array creation
   */ 
  @inline override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[ImmutableArrayProxy[A], B, That]): That = err
/*    if (index != (len - 1))
      err
    else {
      display0(lo - 1) = elem.asInstanceOf[AnyRef]
      this.asInstanceOf[That]
    }
*/
  override def iterator() = err

  def ar = err

  def immutable : ImmutableArrayProxy[A] = {
    val size = len

    import scala.annotation.switch	

    val res : ImmutableArrayProxy[A] = 
      if (len > ImmutableArrayProxyBuilder.vectorAfter) {
	if (size == 0)
	  VectorImpl(Vector.empty)
	else {
	  val s = new Vector[A](0, size, 0)
	  s.initFrom(this)
	  if (depth > 1) s.gotoPos(0, size - 1 )

	  VectorImpl(s)
	}
      } else 
	(len : @switch) match {
	  case 0 => 
	      ImmutableArrayProxy.emptyImmutableArray.asInstanceOf[ImmutableArrayProxy[A]]
	  case 1 =>
	    IAOne(display0(0).asInstanceOf[A])
	  case 2 =>
	    IATwo(display0(0).asInstanceOf[A], display0(1).asInstanceOf[A])
	  case 3 =>
	    IAThree(display0(0).asInstanceOf[A], display0(1).asInstanceOf[A], display0(2).asInstanceOf[A])
	  case _ => 
	    if (copyArray) {
	      val nar = new Array[AnyRef](size)
	      Array.copy(display0, 0, nar, 0, size)
	      ImmutableArrayAll[A](nar)
	    } else
	      ImmutableArray(display0, 0, size) // bad memory wise but no added cost
	}
      
    
    // wipe this one out
    display0 = null.asInstanceOf[Array[AnyRef]]

    res
  }

  override def toString() = "MutableVectorLike"
}

}
