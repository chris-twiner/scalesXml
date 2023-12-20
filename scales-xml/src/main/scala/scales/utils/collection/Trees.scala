package scales.utils.collection

import scala.collection.generic.CanBuildFrom

import scales.utils.{LeftLike, RightLike, EitherLike}

trait Trees {

  type ItemOrTree[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]] = EitherLike[Item, Tree[Item, Section, CC]]

  /** badly named the boolean should indicate if it has any children */
  type ItemOrSectionWalk[Item, Section] = Either[Item, SectionWalk[Section]]

  type TreeCBF[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]] = CanBuildFrom[CC[_], ItemOrTree[Item, Section, CC], CC[ItemOrTree[Item, Section, CC]]]

  final def fold[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X], A](a: A)(folder: (ItemOrSectionWalk[Item, Section], A) => A)(tree: Tree[Item, Section, CC]): A =
    tree.fold(a)(folder)

}

import scales.utils.{ItemOrTree, ItemOrSectionWalk}

/**
 * IF hasChildren then isStart indicates that this particular occurence
 * is the start of the element or the end
 *
 * @author Chris
 *
 */
case class SectionWalk[Section](section: Section, hasChildren: Boolean = false, isStart: Boolean = true)


object Tree {
  def apply[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](isection: Section, ichildren: CC[ItemOrTree[Item, Section, CC]]) : Tree[Item, Section, CC] = new Tree[Item, Section, CC] {
    val section = isection
    val children = ichildren

    def copy( section : Section = section, children : CC[ItemOrTree[Item,Section,CC]] = children) = apply(section, children)

  }

  def unapply[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]]( t : Tree[Item, Section, CC] ) : Option[(Section, CC[ItemOrTree[Item,Section,CC]])] = Some((t.section, t.children))
}


trait Tree[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]] extends RightLike[Item, Tree[Item, Section, CC]] {

  def section: Section
  def children: CC[ItemOrTree[Item, Section, CC]]

  def copy( section : Section = section, children : CC[ItemOrTree[Item,Section,CC]] = children) : Tree[Item, Section, CC]

  def fold[A](a: A)(folder: (ItemOrSectionWalk[Item, Section], A) => A): A = {

    // @scala.annotation.tailrec can't optimize
    def ifold(a: A, folder: (ItemOrSectionWalk[Item, Section], A) => A,
      tree: Tree[Item, Section, CC]): A =
      // match against
      tree match {
        case Tree(top, x) if (x.seq.isEmpty) => folder(Right(SectionWalk(top)), a)
        case Tree(top, children) =>
          val temp = folder(Right(SectionWalk(top, hasChildren = true)), a)
          val fres = children.seq.foldLeft(temp) { (foldeda, iort) =>
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

/*
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
*/
