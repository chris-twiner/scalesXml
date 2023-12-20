package scales.utils.collection.path

import scala.collection.generic.CanBuildFrom
import scales.utils.collection.{SeqLikeThing, Tree}
import scales.utils.{FoldR, ItemOrTree, LeftLike, PathFoldR, TreeCBF, deepestLast, top}

/**
 * Represents the base for operations that fold over a list of paths
 */ 
sealed trait FoldOperation[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]] {

  protected def rootChangeAllowed = false

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC]

  protected def add(path: Path[Item, Section, CC], direction: Int, newPath: Iterable[ItemOrTree[Item, Section, CC]])(implicit cbf : TreeCBF[Item, Section, CC]) : FoldR[Item, Section, CC] = {
    // need to go up to replace
    val parent = path.zipUp
    if (path.top.isLeft && !rootChangeAllowed)
      Right(AddedBeforeOrAfterRoot)
    else
      Left(parent.
        modify { x =>
          val tree = x.right.get;
          val index = path.node.index + direction
          val (pre,pos) = tree.children.splitAt(index)
          val newChildren = (pre ++ newPath) ++ pos
          Tree(tree.section, newChildren.asInstanceOf[CC[ItemOrTree[Item, Section, CC]]]) // actually the same
        })

  }
}

case class Remove[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]]()(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = {
    val ores = path.removeAndUp();
    if (ores.isDefined) Left(ores.get)
    else Right(RemovedRoot)
  }
}

case class AddBefore[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](newPath: ItemOrTree[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = add(path, 0, List(newPath))
}

case class AddAfter[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](newPath: ItemOrTree[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = add(path, 1, List(newPath))
}

/**
 * Use to make it easier to filter out large sets (for those that aren't interesting simply asis them, see tests for use case)
 */
case class AsIs[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]]() extends FoldOperation[Item, Section, CC] {

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = Left(path)
}

object Replace {
  /**
   * Simpler interface
   */
  def apply[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](replaceWith: ItemOrTree[Item, Section, CC]*)(implicit cbf : TreeCBF[Item, Section, CC]) = new Replace[Item, Section, CC](replaceWith)

}

/**
 * Allows replacing one path with many, may be easier to use the * version however
 */
case class Replace[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](replaceWith: Iterable[ItemOrTree[Item, Section, CC]])(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {
  override def rootChangeAllowed = true

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] = {
    // modify with tail
    val tpath = path.modify(_ => replaceWith.head)
    add(tpath, 1, replaceWith.tail)
  }
}

/**
 * Allows foldPositions to be nested, only replace and delete makes sense here (afaict).
 *
 * As such, when wholeTree is false, the path (which must be a tree) is transformed
 * 
 *     p => top(p.tree)
 *
 * and a special case for "deletes" is made - when RemovedRoot is returned from the transformation a delete will take place on the Path.  This enforces that only replace and removes are possible and function appropriately.
 *
 * Warning:
 * 
 * When wholeTree is true the function f is passed the Path (or item) in the original tree, any transformations are then conusmed across the whole tree, which is likely not desired. 
 */
case class ReplaceWith[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](f: PathFoldR[Item, Section, CC], wholeTree : Boolean = false)(implicit cbf : TreeCBF[Item, Section, CC]) extends FoldOperation[Item, Section, CC] {

  def perform(path: Path[Item, Section, CC]): FoldR[Item, Section, CC] =
    // modify back in (allows changes), or pass on the error
    f( if (wholeTree) 
      	 path
       else
         top(path.tree) ).
            fold(fres =>
                Left(path.modify(_ =>
                        fres.tree
                  )),
              x => {
                if ((x eq RemovedRoot) &&
                    (!wholeTree))
                  // delete the node - special case as per doc
                  Remove().perform(path) // it might itself be the root but thats fine
                else
                  Right(x)
              }
            )

}

sealed trait FoldError

case object NoPaths extends FoldError
case object NoSingleRoot extends FoldError
case object RemovedRoot extends FoldError
case object AddedBeforeOrAfterRoot extends FoldError
