package scales.utils

import scalaz.Equal

/**
 * Provides evidence that two instances of two disctinct types are equivalent after a conversion.
 * Requires an instance of Equal for the common convertable type.
 * 
 * @author Derek Williams
 */ 
class Equiv[A : Equal] {
  def apply[ B <% A, C <% A]( b : B , c : C) : Boolean = implicitly[Equal[A]].equal(b:A, c:A)
}

trait EquivFunctions {

  /**
   * Are these two parameters (convertable to a C) equal for a given Equiv[C] instance
   */ 
  def equivalent[A, B, C]( a : A, b : B)(implicit equiv: Equiv[C], viewA: A => C, viewB: B => C) = equiv(a, b)

}
