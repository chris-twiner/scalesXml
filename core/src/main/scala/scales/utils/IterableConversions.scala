package scales.utils

/**
 * Alot of this has since been included in the Scala standard lib, as such its pretty much unused, included incase.
 * 
 * General Java Iterable converters, allowing for comprehensions.
 * 
 * Based on suggestions from Jamie Webb (http://scala.sygneca.com/)
 * will be removed in 0.5
 * @deprecated
 */
object IterableConversions {

  case class JavaIteratorIterator[A](itr: java.util.Iterator[A]) extends Iterator[A] {
    def hasNext = itr.hasNext
    def next() = itr.next
  }

  case class JavaIteratableIterable[A](iterable: java.lang.Iterable[A]) extends Iterable[A] {
    def iterator = JavaIteratorIterator(iterable.iterator)
  }

  implicit def implicitJavaIterableToScalaIterable[A](iterable: java.lang.Iterable[A]): Iterable[A] =
    JavaIteratableIterable(iterable)

  case class JavaEnumerationIterator[A](itr: java.util.Enumeration[A]) extends Iterator[A] {
    def hasNext = itr.hasMoreElements
    def next() = itr.nextElement
  }

  case class JavaEnumerationIterable[A](iterable: java.util.Enumeration[A]) extends Iterable[A] {
    def iterator = JavaEnumerationIterator(iterable)
  }

  implicit def implicitJavaEnumerationToScalaIterable[A](iterable: java.util.Enumeration[A]): Iterable[A] =
    JavaEnumerationIterable(iterable)

}

/**
 * See normal Scala lib, its got alot more in nowadays...
 */
object JavaEnumerationConversions {
  implicit def toEnumeration[A](iterator: java.util.Iterator[A]): java.util.Enumeration[A] =
    new java.util.Enumeration[A] {
      def hasMoreElements = iterator.hasNext
      def nextElement = iterator.next
    }
}

/**
 * General Scala to Iterable converters, allowing for java use.
 * 
 */
object JavaIterableConversions {

  class JavaIterator[T](itr: Iterator[T]) extends java.util.Iterator[T] {
    def hasNext() = itr.hasNext
    def next() = itr.next
    def remove() = throw new java.lang.UnsupportedOperationException("Remove is not implemented in JavaIterableConversions:JavaIterator")
  }

  class JavaIterable[T](iterable: Iterable[T]) extends java.lang.Iterable[T] {
    def iterator() = new JavaIterator[T](iterable.iterator)
  }

  implicit def implicitScalaIterableToJavaIterable[T](iterable: Iterable[T]): java.lang.Iterable[T] =
    new JavaIterable[T](iterable)

}
