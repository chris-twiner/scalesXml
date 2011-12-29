package scales.utils

import scala.collection.immutable.Stack
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom

/**
 * Represents the base for operations that fold over a list of paths
 */ 
sealed trait FoldOperation[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] {

  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]]

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC]

  def add(path: Path[Item, Section, CC], direction: Int, newPath: Iterable[ItemOrTree[Item, Section, CC]])(implicit cbf : TreeCBF[Item, Section, CC]) : FoldR[Item, Section, CC] = {
    // need to go up to replace
    val parent = path.zipUp
    if (path.top.isLeft)
      Right(AddedBeforeOrAfterRoot)
    else
      Left(parent.
        modify { x =>
          val tree = x.right.get;
          val index = path.node.index + direction
          val (pre,pos) = tree.children.splitAt(index)
          val newChildren = (pre ++ newPath) ++ pos
          Tree(tree.section, newChildren)
        })

  }
}

case class Remove[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {
  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = cleanBelow(positions).map { shiftWithBase(positions.head, _, -1) }

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = {
    val ores = path.removeAndUp();
    if (ores.isDefined) Left(ores.get)
    else Right(RemovedRoot)
  }
}

case class AddBefore[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](newPath: ItemOrTree[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {
  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = positions.tail.map { shiftWithBase(positions.head, _, 1) }

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = add(path, 0, List(newPath))
}

case class AddAfter[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](newPath: ItemOrTree[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {
  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = positions.tail.map { shiftWithBase(positions.head, _, 1) }

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = add(path, 1, List(newPath))
}

/**
 * Use to make it easier to filter out large sets (for those that aren't interesting simply asis them, see tests for use case)
 */
case class AsIs[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]]() extends FoldOperation[Item, Section, CC] {
  /**
   * No OP, just move one one
   */
  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = positions.tail

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = Left(path)
}

object Replace {
  /**
   * Simpler interface
   */
  def apply[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](replaceWith: ItemOrTree[Item, Section, CC]*)(implicit cbf : TreeCBF[Item, Section, CC]) = new Replace[Item, Section, CC](replaceWith)

}

/**
 * Allows replacing one path with many, may be easier to use the * version however
 */
case class Replace[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](replaceWith: Iterable[ItemOrTree[Item, Section, CC]])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  /**
   * clean below for any that are invalidated by the replace then replaceWiths size minus the 1 that we have removed/replaced
   */
  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = cleanBelow(positions).map { shiftWithBase(positions.head, _, replaceWith.size - 1) }

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = {
    // modify with tail
    val tpath = path.modify(_ => replaceWith.head)
    add(tpath, 1, replaceWith.tail)
  }
}

/**
 * Allows foldPositions to be nested, only replace makes sense here (afaict)
 */
case class ReplaceWith[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](f: PathFoldR[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  def adjust(positions: Seq[Position[Item, Section, CC]]): Seq[Position[Item, Section, CC]] = cleanBelow(positions).map { shiftWithBase(positions.head, _, 0) }

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] =
    // modify back in (allows changes), or pass on the error
    f(path).fold(fres => Left(path.modify(_ => fres.tree)),
      Right(_))

}

sealed trait FoldError

case object NoPaths extends FoldError
case object NoSingleRoot extends FoldError
case object RemovedRoot extends FoldError
case object AddedBeforeOrAfterRoot extends FoldError
