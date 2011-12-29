package scales

package object utils extends IterableUtils 
  with AsBooleanTrait
  with StackUtils
  with Iteratees
  with Trees
  with Paths
  with ConcurrentMapUtils
  with LocalisedFunctions
{
  def error(str : String) = Predef.error(str)

// https://issues.scala-lang.org/browse/SI-4767 forces them to be here if we want them inlining

  import scala.collection.IndexedSeqLike
  import scala.collection.generic.CanBuildFrom

  @inline final def item[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[X] <: IndexedSeqLike[X, CC[X]]](item: Item): ItemOrTree[Item, Section, CC] = item//Left[Item, Tree[Item, Section, CC]](item)

  @inline final def subtree[Item <: LeftLike[Item, Tree[Item, Section, CC]], Section, CC[A] <: IndexedSeqLike[A, CC[A]]](section: Section, children: CC[ItemOrTree[Item, Section, CC]])(implicit cbf : TreeCBF[Item, Section, CC]): ItemOrTree[Item, Section, CC] = Tree[Item, Section, CC](section, children)//Right[Item, Tree[Item, Section, CC]](Tree[Item, Section, CC](section, children))

  @inline final def one[T]( i : T ) : List[T] =
      i :: Nil

  /**
   * Simple grabber of resources
   */ 
  def resource(a : AnyRef, path : String) =
    a.getClass.getResource(path)
}
