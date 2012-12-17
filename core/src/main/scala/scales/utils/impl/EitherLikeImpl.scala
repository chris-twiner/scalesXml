package scales.utils.impl


trait LeftLikeProjection[+L] {
  def get : L
}

trait RightLikeProjection[+R] {
  def get : R
}

object NonLeftP extends LeftLikeProjection[Nothing] {
  def get : Nothing = error("Cannot get a left from a RightLike.")
}

object NonRightP extends RightLikeProjection[Nothing] {
  def get : Nothing = error("Cannot get a right from a LeftLike.")
}

