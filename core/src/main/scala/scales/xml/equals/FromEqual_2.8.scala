package scales.xml.equals

import scalaz._
import Scalaz._

trait FromEqualsImplicit { }

/**
 * 2.8.x crashes when this is in scope
 */ 
object FromEqualsImplicit {
  /**
   * An implicit but its only purpose is to convert, and needs the given comparison to function, which is provided (or not) by ScalesXml.
   */ 
  implicit def fromCompToEq[T](implicit comp : XmlComparison[T]) : Equal[T] = 
    equal {
      ( a : T, b : T) =>
	comp.compare(false, ComparisonContext.empty, a, b).isEmpty
    }
}
