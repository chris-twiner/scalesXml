package scales.utils.collection

import scala.collection.mutable

trait BuilderHelper[X, CC] {
  /*
   cbf() doesn't exist any more PT <: Iterable[XmlPath]
  def empty: Iterable[XmlPath] = cbf().result
  def just(only: XmlPath): Iterable[XmlPath] = (cbf() += only).result
   */

  def builder: mutable.Builder[X, CC]
}

object BuilderHelper {
  implicit def listBuilderHelper[A]: BuilderHelper[A, List[A]] = new BuilderHelper[A, List[A]] {

    override def builder: mutable.Builder[A, List[A]] = List.empty[A].companion.newBuilder

  }
}
