package scales.xml.impl

import scales.utils.collection.{array, ArraySet, ArraySetsFactory}
import scales.utils.collection.array._

import scales.xml.Attribute

object AttributeSetImplHelper {
  val arrM = implicitly[ClassManifest[Attribute]]
}

trait AttributesImpl extends ArraySetsFactory[Attribute]{
  type A = Attribute
  
  /**
   * @returns the equality Equal type class instance used
   */ 
  def equal: scalaz.Equal[A] = EqualsHelpers.aqnameEqual

  implicit def arrayManifest: ClassManifest[A] = AttributeSetImplHelper.arrM

  def emptySet: ArraySet[A] = AttributeSet.empty

  def one(a: A): ArraySet[A] = new ArraySetOne[A] with AttributesImpl {
    val one = a
  }

  def two(a: A, b: A): ArraySet[A] = new ArraySetTwo[A] with AttributesImpl {
    val one = a
    val two = b
  }

  def three(a: A, b: A, c: A): ArraySet[A] = new ArraySetThree[A] with AttributesImpl {
    val one = a
    val two = b
    val three = c
  }

  def four(a: A, b: A, c: A, d: A): ArraySet[A] = new ArraySetFour[A] with AttributesImpl {
    val one = a
    val two = b
    val three = c
    val four = d
  }

  def five(a: A, b: A, c: A, d: A, e: A): ArraySet[A] = new ArraySetFive[A] with AttributesImpl {
    val one = a
    val two = b
    val three = c
    val four = d
    val five = e
  }

  def more(allTheAs: Array[A]): ArraySet[A] = new ArraySetArray[A] with AttributesImpl {
    val ar = allTheAs
  }
}

object AttributeSet extends AttributesImpl {
  val empty: ArraySet[Attribute] = new EmptyArraySet[A] with AttributesImpl {}

  /**
   * Creates an ArraySet based on the input array and size but does so without checking set membership.
   *
   * When an underlying array must be used the input array is copied unless own is true and len is equal to ar.length
   * 
   */
  def unsafe(ar: Array[Attribute], len: Int, own: Boolean = false): ArraySet[Attribute] =
    len match {
      case 1 => one(ar(0))
      case 2 => two(ar(0), ar(1))
      case 3 => three(ar(0), ar(1), ar(2))
      case 4 => four(ar(0), ar(1), ar(2), ar(3))
      case 5 => five(ar(0), ar(1), ar(2), ar(3), ar(4))
      case _ if (len <= 0) => empty
      case _ => 
	if (own && (ar.length == len)) {
	  more(ar)
	} else {
	  // take the hit
	  val na = Array.ofDim(len)
          Array.copy(ar, 0, na, 0, len)
	  more(na)
	}
    }

}
