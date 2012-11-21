package scales.utils.io

/**
 * Simple option adt to allow handling the difference between a result and an operation that needs more data.
 * NB It exists for clearer types.
 */ 
sealed trait AsyncOption[+T] {
  def fold[R]( empty: => R, hasData: T => R): R

  def map[R]( f: T => R ): AsyncOption[R] = 
    fold(NeedsMoreData, t => HasResult(f(t)))

  def foreach( f: T => Unit): Unit =
    fold((), t => f(t))

  def flatMap[R]( f: T => AsyncOption[R] ): AsyncOption[R] =
    fold(NeedsMoreData, t => f(t))

  def getOrElse[R >: T](thunk: => R): R =
    fold(thunk, t => t)
}

case object NeedsMoreData extends AsyncOption[Nothing] {
  def fold[R]( empty: => R, hasData: Nothing => R): R =
    empty
}

final case class HasResult[T](data: T) extends AsyncOption[T] {
  def fold[R]( empty: => R, hasData: T => R): R =
    hasData(data)
}
