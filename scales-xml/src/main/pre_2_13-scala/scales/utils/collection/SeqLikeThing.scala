package scales.utils.collection

import scala.collection.generic.CanBuildFrom
import scala.collection.{GenTraversableOnce, mutable}

/**
 * 2.13 removes seqlike etc. so we need to behave like it, it also deprecates canbuild fromm.  This interface represents the operations on a seq used by scales
 *
 * @tparam X[_] a seq like container
 */
trait SeqLikeThing[Repr, A, X[_]] {

  implicit def cbf: CanBuildFrom[Repr, A, X[A]]

  @inline def apply(x: X[A])(i: Int): A = seq(x).apply(i)

  @inline def seq(x: X[A]): Seq[A]

  @inline def iterator(x: X[A]): Iterator[A] = seq(x).iterator

  @inline def updated(x: X[A])(index: Int, elem: A): X[A] =
    wrap(seq(x).updated(index, elem))

  @inline def splitAt(x: X[A])(index: Int): (X[A],X[A]) = {
    val (b, a) = seq(x: X[A]).splitAt(index)
    (wrap(b), wrap(a))
  }

  @inline def length(x: X[A]): Int = seq(x: X[A]).length

  @inline def ++(x: X[A])(itr: GenTraversableOnce[A]): X[A] =
    wrap(seq(x: X[A]) ++ itr)

  @inline def wrap(s: Seq[A]): X[A]

  @inline def :+(x: X[A])(elem: A): X[A] =
    wrap(seq(x: X[A]).:+(elem))

  @inline def dropRight(x: X[A])(n: Int): X[A] =
    wrap(seq(x: X[A]).dropRight(n))

  @inline def filter(x: X[A])(p: A => Boolean): X[A] =
    wrap(seq(x: X[A]).filter(p))

  @inline def filterNot(x: X[A])(p: A => Boolean): X[A] =
    wrap(seq(x: X[A]).filterNot(p))

  @inline def builder: mutable.Builder[A, X[A]]
}

case class ImmutableArrayProxyLikeThing[Repr, A](implicit val cbf: CanBuildFrom[Repr, A, ImmutableArrayProxy[A]]) extends SeqLikeThing[Repr, A, ImmutableArrayProxy] {

  override def seq(x: ImmutableArrayProxy[A]): Seq[A] = x

  override def wrap(s: Seq[A]): ImmutableArrayProxy[A] =
    s.asInstanceOf[ImmutableArrayProxy[A]]

  override def builder: mutable.Builder[A, ImmutableArrayProxy[A]] =
    ImmutableArrayProxy.newBuilder

}

case class IndexedSeqLikeThing[Repr, A](implicit val cbf: CanBuildFrom[Repr, A, IndexedSeq[A]]) extends SeqLikeThing[Repr, A, IndexedSeq] {

  override def seq(x: IndexedSeq[A]): Seq[A] = x

  override def wrap(s: Seq[A]): IndexedSeq[A] = s.asInstanceOf[IndexedSeq[A]]

  override def builder: mutable.Builder[A, IndexedSeq[A]] = IndexedSeq.newBuilder
}

case class ListSeqLikeThing[Repr, A](implicit val cbf: CanBuildFrom[Repr, A, List[A]]) extends SeqLikeThing[Repr, A, List] {

  override def seq(x: List[A]): Seq[A] = x

  override def wrap(s: Seq[A]): List[A] = s.asInstanceOf[List[A]]

  override def builder: mutable.Builder[A, List[A]] = List.newBuilder
}

object SeqLikeThing {

  implicit def immutableArrayProxyLikeThing[Repr, A](implicit canBuildFrom: CanBuildFrom[Repr, A, ImmutableArrayProxy[A]]): SeqLikeThing[Repr, A, ImmutableArrayProxy] =
    ImmutableArrayProxyLikeThing()
  implicit def indexedSeqLikeThing[Repr, A](implicit canBuildFrom: CanBuildFrom[Repr, A, IndexedSeq[A]]): SeqLikeThing[Repr, A, IndexedSeq] =
    IndexedSeqLikeThing()

  implicit def listSeqLikeThing[Repr, A](implicit canBuildFrom: CanBuildFrom[Repr, A, List[A]]): SeqLikeThing[Repr, A, List] =
    ListSeqLikeThing()
}
