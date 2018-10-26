package scales.xml.equals

import scalaz.Equal

trait FromEqualsImplicit {

  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }
  /**
   * An implicit but its only purpose is to convert, and needs the given comparison to function, which is provided (or not) by ScalesXml.
   */ 
  implicit def fromCompToEq[T](implicit comp : XmlComparison[T]) : Equal[T] = 
    equal {
      ( a : T, b : T) =>
	comp.compare(false, ComparisonContext.empty, a, b).isEmpty
    }
}
