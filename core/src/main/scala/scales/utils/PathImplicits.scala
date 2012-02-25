package scales.utils

import scala.collection.immutable.Stack
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom

import scalaz._
import Scalaz._

/**
 * Provide & combinator to pass the result of one fold onto the other,
 * in the case of failure no further joined functions will be called.
 *
 * And provides | which allows NoPaths failures, allowing the use site to decide
 * how to combine
 */
class PathFoldCombiner[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](f: PathFoldR[Item, Section, CC]) {
  
  private[this] def onSuccess(next: PathFoldR[Item, Section, CC], orOnFail: (Path[Item, Section, CC], FoldError) => FoldR[Item, Section, CC] = (a, b) => Right(b)): PathFoldR[Item, Section, CC] =
    (path: Path[Item, Section, CC]) =>
      // modify back in (allows changes), or pass on the error
      f(path).fold(fres =>
        next(path.modify(_ => fres.tree)),
	    orOnFail(path, _))

  /**
   * Combine with next, but only when this PathFoldR has not failed
   */ 
  def &(next: PathFoldR[Item, Section, CC]): PathFoldR[Item, Section, CC] =
    onSuccess(next)

  /**
   * Combine with next, and allow next to be used if this PathFoldR returns NoPaths
   */ 
  def |(next: PathFoldR[Item, Section, CC]): PathFoldR[Item, Section, CC] =
    onSuccess(next, orOnFail = (path, res) =>
      if (res eq NoPaths)
        next(path)
      else
        Right(res))
}


trait PathImplicits {
  /**
   * Provide & combinator to pass the result of one fold onto the other,
   * in the case of failure no further joined functions will be called.
   *
   * And provides | which allows NoPaths failures, allowing the use site to decide
   * how to combine
   */
  implicit def fToFoldRToCombine[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](f: PathFoldR[Item, Section, CC]) = new PathFoldCombiner(f)

  implicit def toEqual[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] : Equal[Path[Item, Section, CC]] =
    equal {
      comparePathsDirect(_,_)
    }
}
