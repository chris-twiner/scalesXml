package scales.utils

/**
 * Heavily borrowed from scalaz Ephemeral but I really like my stack and ++
 * If I get it working I'll ask to patch..
 */
object WeakStream {

  def lTo(lower: Long, upper: Long): WeakStream[Long] =
    if (lower > upper) empty else WeakStream.cons(lower, lTo(lower + 1, upper))

  def iTo(lower: Int, upper: Int): WeakStream[Int] =
    if (lower > upper) empty else WeakStream.cons(lower, iTo(lower + 1, upper))

  def iterTo[A](iterator: Iterator[A]): WeakStream[A] =
    if (iterator.isEmpty) empty else {
      val next = iterator.next()
      val after = iterTo(iterator)
      WeakStream.cons(next, after)
    }

  def empty[A] = new WeakStream[A]{
    val empty = true
    def head = error("empty")
    def tail = error("empty")
  }

  object cons {
    def apply[A](a: => A, as: => WeakStream[A]) = new WeakStream[A] {
      val empty = false
      val head = weakMemo(a)
      val tail = weakMemo(as)
    }
  }

  def apply[A](a : A, as : A *) : WeakStream[A] = new WeakStream[A]{
    val empty = false
    val head = weakMemo(a)
    def tail =
      weakMemo{
        if (as.isEmpty) WeakStream.empty
        else {
          val astail = as.tail;
          cons(as.head, if (astail.isEmpty) WeakStream.empty else
            apply(astail.head,
              astail.tail :_*))
        }
      }
  }


  object ##:: {
    def unapply[A](xs: WeakStream[A]): Option[(A, WeakStream[A])] =
      if (xs.isEmpty) None
      else Some((xs.head(), xs.tail()))
  }

  implicit def toIterable[A](e: WeakStream[A]): Iterable[A] = new Iterable[A] {
    def iterator = new Iterator[A] {
      var cur = e
      def next = {
        val t = cur.head()
        cur = cur.tail()
        t
      }
      def hasNext = !cur.empty
    }
  }

  def append[A, B >: A]( a : WeakStream[A], e : => WeakStream[B] ) : WeakStream[B] =
    if (!a.empty) cons(a.head(), append(a.tail(), e))
    else e

  def weakMemo[V](f: => V): () => V = {
    import java.lang.ref.{WeakReference, SoftReference}

    val latch = new Object
    @volatile var v: Option[WeakReference[V]] = None
    () => {
      val a = v.map(x => x.get)
      if (a.isDefined && a.get != null) a.get
      else
        latch.synchronized {
          val x = f
          v = Some(new WeakReference(x))
          x
        }
    }
  }
}

sealed trait WeakStream[+A] {

  val empty : Boolean

  //    def ++( e : => WeakStream[A]) : WeakStream[A] = WeakStream.append(this, e)
  def ++[B >: A]( e : => WeakStream[B]) : WeakStream[B] = WeakStream.append[A, B](this, e)

  def head : () => A
  def tail : () => WeakStream[A]
}
