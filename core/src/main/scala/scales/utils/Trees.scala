package scales.utils

import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom

trait Trees {

  type ItemOrTree[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] = EitherLike[Item, Tree[Item, Section, CC]]

  /** badly named the boolean should indicate if it has any children */
  type ItemOrSectionWalk[Item, Section] = Either[Item, SectionWalk[Section]]

  type TreeCBF[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] = CanBuildFrom[CC[_], ItemOrTree[Item, Section, CC], CC[ItemOrTree[Item, Section, CC]]]

  final def fold[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], A](a: A)(folder: (ItemOrSectionWalk[Item, Section], A) => A)(tree: Tree[Item, Section, CC]): A =
    tree.fold(a)(folder)

}

/**
 * IF hasChildren then isStart indicates that this particular occurence
 * is the start of the element or the end
 *
 * @author Chris
 *
 */
case class SectionWalk[Section](section: Section, hasChildren: Boolean = false, isStart: Boolean = true)

case class Tree[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[C] <: IndexedSeqLike[C, CC[C]]](section: Section, children: CC[ItemOrTree[Item, Section, CC]]) extends RightLike[Item, Tree[Item, Section, CC]]
//    (implicit cbf : TreeCBF[Item, Section, CC]) 
{
  def fold[A](a: A)(folder: (ItemOrSectionWalk[Item, Section], A) => A): A = {
    // @scala.annotation.tailrec can't optimize
    def ifold(a: A, folder: (ItemOrSectionWalk[Item, Section], A) => A,
      tree: Tree[Item, Section, CC]): A =
      // match against
      tree match {
        case Tree(top, x) if (x.isEmpty) => folder(Right(SectionWalk(top)), a)
        case Tree(top, children) =>
          val temp = folder(Right(SectionWalk(top, hasChildren = true)), a)
          val fres = children.foldLeft(temp) { (foldeda, iort) =>
            iort.fold( 
	      (item : Item) => folder(Left(item), foldeda), 
	      (rtree : Tree[Item,Section,CC]) => ifold(foldeda, folder, rtree)
	    )
          }
          folder(Right(SectionWalk(top, hasChildren = true, isStart = false)), fres)
      }
    ifold(a, folder, this)
  }
}
