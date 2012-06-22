package scales.utils

import scala.collection.immutable.Stack
import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom

object PathFold {

  /**
   * Folds over positions within a single path, for example all given children.  As such positions must be calculated.
   *
   * Takes the first root, returning Right(NoSingleRoot) if any of the subsequent roots don't match.
   *
   * folder retrieves the current path
   *
   * Each iteration folds the resulting tree back into the path. As this function must maintain the Path it does not expose the new path root until the result.
   *
   * The progress through the document is in reverse document order.  This ensures that transformations can always be safely composed, e.g. a delete of a path won't stop changes below it.  This, however, implies the developer must also handle any accumalation in "reverse". 
   */
  def foldPositions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], ACC](locations: Iterable[Path[Item, Section, CC]], accumulator: ACC)(folder: (ACC, Path[Item, Section, CC]) => (ACC, FoldOperation[Item, Section, CC])) (implicit cbf : TreeCBF[Item, Section, CC], cm : ClassManifest[(scales.utils.Position[Item,Section,CC], Path[Item, Section, CC])])  : Either[(ACC, Path[Item, Section, CC]), FoldError] = {
    if (locations.isEmpty) return Right(NoPaths)

    val sorted = sortPositions(locations, false)

    val head = sorted.head
    var accum = accumulator

    val rootPosition = head._1
    val differentRoot = sorted.exists(p => p._1.root ne rootPosition.root)
    if (differentRoot)
      Right(NoSingleRoot)
    else {

      def withPositions( opositions : Seq[scales.utils.Position[Item,Section,CC]] ) : Either[(ACC, Path[Item, Section, CC]), FoldError] = {
	var positions = opositions
	var path = head._2
	while (!positions.isEmpty) {

          val (accf, res) = folder(accum, path)
          accum = accf
          val matched = res.perform(path) //matchIt( res, path )

          if (matched.isLeft) {
            path = matched.left.get
            positions = positions.drop(1)
            if (!positions.isEmpty) {
              path = moveTo(path, positions.head) // else nothing we keep path to call root
            }
          } else return Right(matched.right.get)
	}
	Left((accum, rootPath(path)))
      }

      // fold over positions, with the path from head, let each foldop decide what the next position sequence looks like
      var positions = sorted.map(_._1).toSeq
	
      withPositions(positions)
    }
  }

}

/**
 * Utility functions for Paths, sorting, moving between Paths, getting to the root etc.
 */ 
trait Paths {
  def noPath[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]]
    (implicit cbf : TreeCBF[Item, Section, CC])  = new Path[Item, Section, CC](Top(), Node(-1, null.asInstanceOf[ItemOrTree[Item, Section, CC]])) {
  }

  /**
   * Returns the root path for its input, uses zipUp to ensure changes are kept
   */
  def rootPath[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path: Path[Item, Section, CC]): Path[Item, Section, CC] = {
    var newPath = path
    while (!newPath.top.isLeft)
      newPath = newPath.zipUp
    newPath
  }

  /**
   * Navigates the path until the new position is reached, throws if either its a new root or the position is not reachable
   */
  def moveTo[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path: Path[Item, Section, CC], newPos: Position[Item, Section, CC])
    (implicit cbf : TreeCBF[Item, Section, CC]) : Path[Item, Section, CC] = {

    val root = rootPath(path)
    // cheaty way, crap but quick enough
    // TODO come back to this and properly move,
    newPos.position.pop.foldLeft(root) { (path, pos) =>
      Path(path, Node(pos, path.children(pos)))
    }
  }

  type FoldR[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] = Either[Path[Item, Section, CC], FoldError]

  type PathFoldR[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] = (Path[Item, Section, CC]) => FoldR[Item, Section, CC]

  /**
   * As per the non accumalating version, folds over positions within a given tree but allows for an additional accumalation.
   * 
   * The progress through the document is in reverse document order.  This ensures that transformations can always be safely composed, e.g. a delete of a path won't stop changes below it.  This, however, implies the developer must also handle any accumalation in "reverse". 
   */ 
  def foldPositions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], ACC](locations: Iterable[Path[Item, Section, CC]], accumulator: ACC)(folder: (ACC, Path[Item, Section, CC]) => (ACC, FoldOperation[Item, Section, CC])) (implicit cbf : TreeCBF[Item, Section, CC], cm : ClassManifest[(scales.utils.Position[Item,Section,CC], Path[Item, Section, CC])])  : Either[(ACC, Path[Item, Section, CC]), FoldError] = PathFold.foldPositions(locations, accumulator)(folder)(cbf, cm)

  /**
   * Folds over positions within a single path, for example all given children.  As such positions must be calculated.
   *
   * Takes the first root, returning Right(NoSingleRoot) if any of the subsequent roots don't match.
   *
   * folder retrieves the current path
   *
   * Each iteration folds the resulting tree back into the path. As this function must maintain the Path it does not expose the new path root until the result.
   */
  def foldPositions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](locations: Iterable[Path[Item, Section, CC]])(folder: (Path[Item, Section, CC]) => FoldOperation[Item, Section, CC])(implicit cbf : TreeCBF[Item, Section, CC], cm : ClassManifest[(scales.utils.Position[Item,Section,CC], Path[Item, Section, CC])]) : FoldR[Item, Section, CC] =
    foldPositions[Item, Section, CC, Unit](locations, ())((u, p) => ((), folder(p))).
      fold(x => Left(x._2), Right(_))

  val NotSameRoot = 1000

  /**
   * When paths are not in the same root, they are compared based on the identity hash of the given roots.  Of course this relies on that function
   * having a decent vm implementation.
   *
   * @param path1
   * @param path2
   * @return 1 if path1 is before path2, -1 if path2 is before path1, 0 if they are the same and NotSameRoot+-1 if they are not in the same root
   */
  def comparePathPositions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path1: Position[Item, Section, CC], path2: Position[Item, Section, CC]): Int = {

    if (path1 eq path2) 0
    else {
      if (path1.root ne path2.root) {
        val p1R = System.identityHashCode(path1.root)
        val p2R = System.identityHashCode(path2.root)
        NotSameRoot + (if (p1R < p2R) 1 else -1)
      } else
        compareStack(path1.position, path2.position)
    }
  }

  /**
   * Helper for comparePaths, will not evaluate position if the paths are equal
   */
  def comparePathsDirect[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path1: Path[Item, Section, CC], path2: Path[Item, Section, CC]): Boolean =
    if (path1 eq path2)
      true
    else
      comparePathsP((path1.position, path1), (path2.position, path2))._1 == 0

  /**
   * When paths are not in the same root, they are compared based on the identity hash of the given roots.  Of course this relies on that function
   * having a decent vm implementation. See http://www.w3.org/TR/2007/REC-xpath20-20070123/#dt-document-order, tree order must remain constant.  Its
   * also a pretty sensible approach for non xml trees.
   *
   * @param path1
   * @param path2
   * @return 1 if path1 is before path2, -1 if path2 is before path1, 0 if they are the same and NotSameRoot+-1 if they are not in the same root
   */
  def comparePaths[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path1: Path[Item, Section, CC], path2: Path[Item, Section, CC]): (Int, Position[Item, Section, CC], Position[Item, Section, CC]) =
    comparePathsP((path1.position, path1), (path2.position, path2))

  def comparePathsP[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path1: (Position[Item, Section, CC], Path[Item, Section, CC]), path2: (Position[Item, Section, CC], Path[Item, Section, CC])): (Int, Position[Item, Section, CC], Position[Item, Section, CC]) = {
    if (path1._2 eq path2._2) {
      val pos = path1._1
      (0, pos, pos)
    } else {
      val (pos1, pos2) = (path1._1, path2._1)
      (comparePathPositions(pos1, pos2), pos1, pos2)
    }
  }

  def comparePathsT[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], T](path1: (Position[Item, Section, CC], (T, Path[Item, Section, CC])), path2: (Position[Item, Section, CC], (T, Path[Item, Section, CC]))): (Int, Position[Item, Section, CC], Position[Item, Section, CC]) = {
    if (path1._2._2 eq path2._2._2) {
      val pos = path1._1
      (0, pos, pos)
    } else {
      val (pos1, pos2) = (path1._1, path2._1)
      (comparePathPositions(pos1, pos2), pos1, pos2)
    }
  }


  import scalaz.Equal
  import scalaz.Scalaz.equal

  /**
   * Provides an instance of the Equal type class for positional Equality
   */ 
  def toPositionalEqual[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]] : Equal[Path[Item, Section, CC]] =
    equal {
      comparePathsDirect(_,_)
    }

  def top[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](tree: Tree[Item, Section, CC])
    (implicit cbf : TreeCBF[Item, Section, CC]) : Path[Item, Section, CC] =
    Path(Top(), Node(0, tree))

  /**
   * positions with tuples (T, Path)
   */ 
  def positionsT[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], T](paths: Iterable[(T, Path[Item, Section, CC])]): Iterable[(Position[Item, Section, CC], (T,Path[Item, Section, CC]))] =
    paths.map(x => (x._2.position, x))

  /**
   * Obtain the positions for the paths
   */ 
  def positions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](paths: Iterable[Path[Item, Section, CC]]): Iterable[(Position[Item, Section, CC], Path[Item, Section, CC])] =
    paths.map(x => (x.position, x))

  /**
   * sortPositions with a  tuple T, Path
   */
  def sortPositionsT[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], T](paths: Iterable[(T,Path[Item, Section, CC])],
    isDescending: Boolean = true)(implicit cm : ClassManifest[(scales.utils.Position[Item,Section,CC], (T, Path[Item, Section, CC]))]): Iterable[(Position[Item, Section, CC], (T, Path[Item, Section, CC]))] =
    // Have to force them anyway
    scala.util.Sorting.stableSort(positionsT(paths).toSeq, (p1: (Position[Item, Section, CC], (T,Path[Item, Section, CC])), p2: (Position[Item, Section, CC], (T,Path[Item, Section, CC]))) => {
      val (res, pos1, pos2) = comparePathsT(p1, p2)
      val order = (res == 1 || res == (NotSameRoot + 1))
      if (isDescending) order else !order
    })(cm)

  /**
   * Sorts according to position of each path item, descending or descending based on a depth first then rightwise order.
   */
  def sortPositions[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](paths: Iterable[Path[Item, Section, CC]],

    isDescending: Boolean = true)(implicit cm : ClassManifest[(scales.utils.Position[Item,Section,CC], Path[Item, Section, CC])]): Iterable[(Position[Item, Section, CC], Path[Item, Section, CC])] =

    // Have to force them anyway
    scala.util.Sorting.stableSort(positions(paths).toSeq, (p1: (Position[Item, Section, CC], Path[Item, Section, CC]), p2: (Position[Item, Section, CC], Path[Item, Section, CC])) => {
      val (res, pos1, pos2) = comparePathsP(p1, p2) 
      val order = (res == 1 || res == (NotSameRoot + 1))
      if (isDescending) order else !order
    })(cm)
   

  /**
   * Sorts according to position of each path item, descending or descending based on a depth first then rightwise order.
   */
  def sort[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](paths: Iterable[Path[Item, Section, CC]],
    isDescending: Boolean = true)(implicit cm : ClassManifest[(scales.utils.Position[Item,Section,CC], Path[Item, Section, CC])]): Iterable[Path[Item, Section, CC]] = sortPositions(paths, isDescending).map(x => x._2)

  /**
   * sort with a tuple T, Path
   */ 
  def sortT[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]], T](paths: Iterable[(T,Path[Item, Section, CC])],
    isDescending: Boolean = true)(implicit cm : ClassManifest[(scales.utils.Position[Item,Section,CC], (T, Path[Item, Section, CC]))]): Iterable[(T,Path[Item, Section, CC])] = sortPositionsT(paths, isDescending).map(x => x._2)

  /**
   * Deepest last child
   */ 
  def deepestLast[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](path : Path[Item, Section, CC]) : Path[Item, Section, CC] = {
    if (path.hasChildren) {
      val npath = path.lastChild.get
      if (npath.isItem)
	npath
      else
	deepestLast(npath) // keep going in 
    } else 
      path
  }

  /**
   * gets the next preceding:: sibling equivalent in document order, unlike XPath preceding:: it does not exclude parents 
   */
  def preceding[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]]( path : Path[Item, Section, CC] ) : Option[Path[Item, Section, CC]] =
    if (path.hasPreviousSibling) {
      val t = path.previousSibling
      Some(
	if (t.isItem)
          t
	else
	  deepestLast(t))
    } else
      if (path.top.isLeft)
	None
      else
	preceding(path.top.getRight) // move up

  /**
   * gets the next following:: sibling in document order 
   */ 
  def following[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]]( path : Path[Item, Section, CC] ) : Option[Path[Item, Section, CC]] =
    if (path.hasNextSibling)
      Some(path.nextSibling)
    else
      if (path.top.isLeft)
	None
      else
	following(path.top.getRight) // move up

}
