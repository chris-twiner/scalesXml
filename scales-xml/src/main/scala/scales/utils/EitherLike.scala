package scales.utils

import impl.{LeftLikeProjection, RightLikeProjection, NonLeftP, NonRightP}

/**
 * Either is great, perfect for what is needed, except for the indirection and the added memory and cpu costs thereof.  If ScalesXml is meant to be used in the placesthat JAXP and friends would live it should perform the best it can.
 *
 * That means reducing allocations wherever possible and optimising up to the point of model damage.  I think this unfortunately crosses that line but meets the aim.
 */ 
sealed trait EitherLike[+L,+R] {
  def isLeft : Boolean

  def isRight : Boolean

  def fold[X]( fl : (L) => X, fr : (R) => X) : X

  def left : LeftLikeProjection[L]
  
  def getLeft : L

  def right : RightLikeProjection[R]

  def getRight : R
}

trait LeftLike[+L, +R] extends LeftLikeProjection[L] with EitherLike[L,R] { lefty : L =>
  @inline final def isLeft = true
  @inline final def isRight = false

  @inline final def fold[X]( fl : (L) => X, fr : (R) => X) : X = fl(lefty)

  @inline final def left : LeftLikeProjection[L] = this

  @inline final def right : RightLikeProjection[R] = NonRightP.asInstanceOf[RightLikeProjection[R]]

  @inline final def get : L = lefty

  @inline final def getLeft : L = lefty

  @inline final def getRight : R = right.get

}

trait RightLike[+L, +R] extends RightLikeProjection[R] with EitherLike[L,R] { righty : R =>
  @inline final def isLeft = false
  @inline final def isRight = true

  @inline final def fold[X]( fl : (L) => X, fr : (R) => X) : X = fr(righty)

  @inline final def left : LeftLikeProjection[L] = NonLeftP.asInstanceOf[LeftLikeProjection[L]]

  @inline final def right : RightLikeProjection[R] = this 

  @inline final def get : R = righty

  @inline final def getRight : R = righty

  @inline final def getLeft : L = left.get
}
