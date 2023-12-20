package scales.utils.collection

import scales.utils.collection

import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.collection.{GenTraversableOnce, mutable}

/**
 * 2.13 removes seqlike etc. so we need to behave like it, it also deprecates canbuild fromm
 *
 * @tparam A
 */
trait SeqLikeThing[A] extends scala.collection.SeqLike[A, SeqLikeThing[A]] {
  type Repr <: SeqLikeThing[A]

  def apply(i: Int): A = seq.apply(i)

  def seq: Seq[A]

  def iterator: Iterator[A] = seq.iterator

  def updated[B >: A, That](index: Int, elem: A)(implicit bf: CanBuildFrom[Repr, A, That]): Repr =
    wrap(seq.updated(index, elem))

  override def splitAt(index: Int): (Repr,Repr) = {
    val (b, a) = seq.splitAt(index)
    (wrap(b).asInstanceOf[Repr], wrap(a).asInstanceOf[Repr])
  }

  override def length: Int = seq.length

  def ++[That](itr: GenTraversableOnce[A])(implicit bf: CanBuildFrom[Repr, A, That]): Repr =
    wrap(seq ++ itr)

  def wrap(s: Seq[A]): Repr

  def :+[That](elem: A)(implicit bf: _root_.scala.collection.generic.CanBuildFrom[Repr, A, That]): Repr =
    wrap(seq.:+(elem))

  override def dropRight(n: Int): Repr =
    wrap(seq.dropRight(n))

  override def filter(p: A => Boolean): Repr =
    wrap(seq.filter(p))

  override def filterNot(p: A => Boolean): Repr =
    wrap(seq.filterNot(p))

  def builder: mutable.Builder[A, SeqLikeThing[A]] = newBuilder
}

case class ImmutableArrayProxyLikeThing[A](immutableArrayProxy: ImmutableArrayProxy[A]) extends SeqLikeThing[A] {
  override type Repr = ImmutableArrayProxyLikeThing[A]

  override def seq: Seq[A] = immutableArrayProxy

  override def wrap(s: Seq[A]): Repr = ImmutableArrayProxyLikeThing(s.asInstanceOf[ImmutableArrayProxy[A]])

  override protected[this] def newBuilder: mutable.Builder[A, SeqLikeThing[A]] =
    SeqLikeThingBuilder[A, ImmutableArrayProxyLikeThing](seq.genericBuilder)
}

case class IndexedSeqLikeThing[A](indexedSeq: IndexedSeq[A]) extends SeqLikeThing[A] {
  override type Repr = IndexedSeqLikeThing[A]

  override def seq: Seq[A] = indexedSeq

  override def wrap(s: Seq[A]): Repr = IndexedSeqLikeThing(s.asInstanceOf[IndexedSeq[A]])

  override protected[this] def newBuilder: mutable.Builder[A, SeqLikeThing[A]] =
    SeqLikeThingBuilder[A, IndexedSeqLikeThing](seq.genericBuilder)
}

case class SeqSeqLikeThing[A](toWrap: Seq[A]) extends SeqLikeThing[A] {
  override type Repr = SeqSeqLikeThing[A]

  override def seq: Seq[A] = toWrap

  override def wrap(s: Seq[A]): Repr = SeqSeqLikeThing(s)

  override protected[this] def newBuilder: mutable.Builder[A, SeqLikeThing[A]] =
    SeqLikeThingBuilder[A, SeqSeqLikeThing](seq.genericBuilder)
}

case class SeqLikeThingBuilder[A, CC[X] <: SeqLikeThing[X]](instance: mutable.Builder[A, Seq[A]])(implicit gen: SeqLikeThingGen[CC[A]]) extends mutable.Builder[A, CC[A]] {

  override def +=(elem: A): SeqLikeThingBuilder.this.type = {
    instance.+=( elem )
    this
  }

  override def clear(): Unit = {
    instance.clear()
  }

  override def result(): CC[A] = implicitly[collection.SeqLikeThingGen[CC[A]]].wrap( instance.result() ).asInstanceOf[CC[A]]
}

trait SeqLikeThingGen[+A] {
  def empty[B]: SeqLikeThing[B]
  def wrap[B](seq: Seq[B]): SeqLikeThing[B]
}

object SeqLikeThingGen {
  implicit def immutableArrayProxyGen[A]: SeqLikeThingGen[ImmutableArrayProxyLikeThing[A]] = new SeqLikeThingGen[ImmutableArrayProxyLikeThing[A]] {

    override def empty[B]: SeqLikeThing[B] = ImmutableArrayProxyLikeThing(ImmutableArrayProxy.empty)

    override def wrap[B](seq: Seq[B]): SeqLikeThing[B] = ImmutableArrayProxyLikeThing(seq.asInstanceOf[ImmutableArrayProxy[B]])
  }

  implicit def indexedSeqGen[A]: SeqLikeThingGen[IndexedSeqLikeThing[A]] = new SeqLikeThingGen[IndexedSeqLikeThing[A]] {

    override def empty[B]: SeqLikeThing[B] = IndexedSeqLikeThing(IndexedSeq.empty)

    override def wrap[B](seq: Seq[B]): SeqLikeThing[B] = IndexedSeqLikeThing(seq.asInstanceOf[IndexedSeq[B]])
  }

  implicit def seqGen[A]: SeqLikeThingGen[SeqSeqLikeThing[A]] = new SeqLikeThingGen[SeqSeqLikeThing[A]] {

    override def empty[B]: SeqLikeThing[B] = SeqSeqLikeThing(Seq.empty)

    override def wrap[B](seq: Seq[B]): SeqLikeThing[B] = SeqSeqLikeThing(seq)
  }

}

case class SeqLikeThingBuildFrom[A, CC[X] <: SeqLikeThing[X]]()(implicit gen: SeqLikeThingGen[CC[A]])  extends CanBuildFrom[CC[_], A, CC[A]] {

  override def apply(from: CC[_]): mutable.Builder[A, CC[A]] = ???

  override def apply(): mutable.Builder[A, CC[A]] =
    SeqLikeThingBuilder(gen.empty.seq.companion.newBuilder)(gen)
}

object SeqLikeThing {

  implicit def buildFrom[A, CC[X] <: SeqLikeThing[X]](implicit gen: SeqLikeThingGen[CC[A]]) =
    SeqLikeThingBuildFrom()(gen)

  def wrap[A](toWrap: Seq[A]): SeqLikeThing[A] =
    toWrap match {
      case t: ImmutableArrayProxy[_] => ImmutableArrayProxyLikeThing(t)
      case t: IndexedSeq[_] => IndexedSeqLikeThing(t)
      case t: Seq[_] => SeqSeqLikeThing(t)
    }
}
