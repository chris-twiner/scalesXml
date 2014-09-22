package scales.xml.equals

import scalaz.Equal
import Equal._

trait FromEqualsImplicit {
  /**
   * An implicit but its only purpose is to convert, and needs the given comparison to function, which is provided (or not) by ScalesXml.
   */ 
  implicit def fromCompToEq[T](implicit comp : XmlComparison[T]) : Equal[T] = 
    equal {
      ( a : T, b : T) =>
	comp.compare(false, ComparisonContext.empty, a, b).isEmpty
    }
}
