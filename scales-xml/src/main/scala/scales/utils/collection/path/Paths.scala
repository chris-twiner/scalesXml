package scales.utils.collection.path

import scala.collection.immutable.Stack
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scales.utils.{EitherLike, ItemOrTree, LeftLike, RightLike, TreeCBF, collection, subtree}
import collection.{SeqLikeThing, Tree}

/**
 * Represents the Top for a given Path, there isn't a tree above this
 */ 
case class Top[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]]() extends LeftLike[Top[Item, Section, CC], Path[Item, Section, CC]]

/**
 * Positions only have meaning for a given Path(s).
 *
 * Their internal representation may change, as such its private to Paths.
 *
 * @author Chris
 *
 */
trait Position[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]] {
  private[utils] val root: Path[Item, Section, CC]
  private[utils] val position: List[Int]
}

private[utils] case class PositionImpl[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](position: List[Int], root: Path[Item, Section, CC]) extends Position[Item, Section, CC]

/**
 * Position in a parent Paths children
 * 
 * @author Chris
 *
 */ // note - lazy is a perf killer for building, probably doesn't save much over interrogation either (given Iterator is used) 
case class Node[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](index: Int, focus: ItemOrTree[Item, Section, CC])

case class Path[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: SeqLikeThing[X]](top: EitherLike[Top[Item,Section,CC], Path[Item, Section, CC]], node: Node[Item, Section, CC])
     (implicit cbf : TreeCBF[Item, Section, CC]) extends Iterable[Path[Item, Section, CC]] with RightLike[Top[Item, Section, CC], Path[Item, Section, CC]] { self =>

  def parentTree : Tree[Item, Section, CC] = top.getRight.node.focus.getRight

  def parentCount = if (top.isLeft) 0 else parentTree.children.seq.length

  /** Is there a previous sibling */
  def hasPreviousSibling = (node.index > 0 && node.index <= parentCount)

  /** Does it have a further sibling */
  def hasNextSibling = (node.index > -1 && node.index < (parentCount - 1))

  /**
   * Is the focus an Item?
   */
  def isItem = node.focus.isLeft

  /**
   * Does the focus have a child?
   */
  def hasChildren =
    if (isItem) false
    else children.seq.length != 0

  /**
   * Folds over child or tree
   */ 
  def focus[R]( i : Item => R, t : Tree[Item,Section,CC] => R) : R =
    node.focus.fold(i, t)

  /**
   * Children for a path, don't call unless it is not an item
   */ 
  def children : CC[ItemOrTree[Item,Section,CC]]  = node.focus.right.get.children

  /**
   * Call hasPreviousSibling first to assert there is a previous sibling
   */
  def previousSibling = Path(top, Node(node.index - 1, parentTree.children.seq.apply(node.index - 1)))

  /**
   * Call hasNextSibling first to assert there is a next sibling
   */
  def nextSibling = Path(top, Node(node.index + 1, parentTree.children.seq(node.index + 1)))

  /**
   * get the tree (isItem == false)
   * @return
   */
  def tree() = node.focus.getRight

  /**
   * get the item (isItem == true)
   * @return
   */
  def item() = node.focus.getLeft

  private[this] class ItemIterator() extends Iterator[Path[Item,Section, CC]] { 
    def hasNext = false
    def next() = scales.utils.error("Cannot iterate over an item")
  }

  private[this] class TreeIterator() extends Iterator[Path[Item,Section, CC]] { 
    val c = children.iterator
    var index = -1
    
    def hasNext = c.hasNext

    def next() : Path[Item, Section, CC] = {
      index += 1
      Path(self, Node(index, c.next()))
    }
  }

  /**
   * Provide iterator to the path of the children
   */
  def iterator() = 
    if (isItem)
      new ItemIterator()
    else
      new TreeIterator()

  /**
   * Returns either the first child or none.  Note the child would still need to be unpacked
   * @return
   */
  def firstChild(): Option[Path[Item, Section, CC]] =
    if (!hasChildren) None // can't have a child if its just a data node
    else Some(Path(this, Node(0, children.seq.head)))

  /**
   * Returns either the last child or none.  Note the child would still need to be unpacked
   * @return
   */
  def lastChild(): Option[Path[Item, Section, CC]] = // don't ask children twice
    if (isItem) None // items don't have children
    else {
      val c = children
      if (c.seq.length == 0) None // no children
      else {
        val newPos = c.seq.length - 1
        Some(Path(this,
          Node(newPos, c.seq(newPos))))
      }
    }
										   
  /**
   * zipUp returns this if it is already the top item (doesn't have a parent to zip), and a
   * newly zipped node when not if the focus has been modified
   */
  def zipUp(): Path[Item, Section, CC] =
    if (top.isLeft) this
    else {
      val path = top.right.get
      val pt = parentTree

      if (pt.children.seq(node.index) eq node.focus) path
      else {
        val parentFocus = // ZIP IT, must be a tree
          subtree[Item, Section, CC](pt.section, pt.children.updated(node.index, node.focus).asInstanceOf[CC[ItemOrTree[Item, Section, CC]]])

        Path(path.top, Node(path.node.index,
          parentFocus))
      }
    }

  /**
   * Replaces this paths focus returning the path for the returned node.
   * @param newFocus transforming function on the ItemOrTree.
   */
  def modify(newFocus: (ItemOrTree[Item, Section, CC]) => ItemOrTree[Item, Section, CC]) =
    Path(top, Node(node.index,
      newFocus(node.focus)))

  /**
   * Removes this node, returning the parent path or None if its top
   * 
   */
  def removeAndUp(): Option[Path[Item, Section, CC]] =
    if (top.isLeft) None
    else {
      val path = top.right.get
      val tree = path.node.focus.right.get

      val parentFocus = {
        val c = parentTree.children

        if (c.seq.size == 1) {
          // optimising for pull parse onqnames, if its only one in the parent, set the parent to empty, sucks if its not traversable
          subtree(tree.section, c.builder.result().asInstanceOf[CC[ItemOrTree[Item, Section, CC]]])
        } else {
          val parts = parentTree.children.splitAt(node.index)
          subtree(tree.section, ((parts._1 ++ parts._2.tail).asInstanceOf[CC[ItemOrTree[Item, Section, CC]]]))
        }
	
      }

      Some(
        Path(path.top, Node(path.node.index,
          parentFocus)))
    }

  /**
   * Gets the relative position of this Path
   * DOES NOT perform a zip
   */
  def position(): Position[Item, Section, CC] = {
    @scala.annotation.tailrec
    def makePosition(path: Path[Item, Section, CC], stack: List[Int]): (List[Int], Path[Item, Section, CC]) = {
      val newStack = path.node.index :: stack
      if (path.top.isLeft)
        (newStack, path)
      else
        makePosition(path.top.right.get, newStack)
    }

    val res = makePosition(this, List[Int]())
    new PositionImpl[Item,Section,CC](res._1, res._2)
  }

  /**
   * Gives a nicer representation then the standard (Paths(Paths(), Paths).  But still too much info
   *
   * Path( Section | Top, FocusSection | FocusItem)
   *
   * @return
   */
  override def toString(): String = {
    val builder = new java.lang.StringBuilder()

    builder.append("Path(");
    if (top.isLeft)
      builder.append("Top");
    else
      builder.append(top.right.get.tree.section);
    builder.append(",");
    builder.append(node.index);
    builder.append(",");
    if (isItem)
      builder.append(item);
    else
      builder.append(tree.section);
    builder.append(")");

    builder.toString
  }
}
