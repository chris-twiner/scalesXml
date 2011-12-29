package scales.utils

/**
 * Provides the conversion type class AsBoolean, and simple matchers (extractor builders) to
 * use them.  Used by the xpath boolean function and xmltree matching
 */ 
trait AsBooleanTrait {
  type AsBoolean[T] = Function1[T, Boolean]
  def boolean[T : AsBoolean]( it : T ) : Boolean = implicitly[AsBoolean[T]].apply(it)
  
  class BooleanMatcher[X, T : AsBoolean](eval  : (X) => T) {
    def unapply( x : X ) : Option[T] = {
      val res = eval(x)
      if (!implicitly[AsBoolean[T]].apply(res)) None
      else Some(res)
    }
  }

  class BooleanAndTMatcher[X, T : AsBoolean](eval : (X) => T) {
    def unapply( x : X ) : Option[(T, X)] = {
      val res = eval(x)
      if (!implicitly[AsBoolean[T]].apply(res)) None
      else Some((res, x))
    }
  }

  /**
   * Calls eval to return a T t'.  This T is then evaluated with the AsBoolean type class.
   *
   * When evaluated to true t' is returned.
   * val Matcher = booleanMatcher( (x) => T ); val Matcher(t) = x
   */ 
  def booleanMatcher[X, T : AsBoolean]( eval : (X) => T ) = new BooleanMatcher[X, T](eval)

  /**
   * Calls eval to return a T t'.  This T is then evaluated with the AsBoolean type class.
   *
   * When evaluated to true (t',x) is returned.
   * val Matcher = booleanMatcher( (x) => T ); val Matcher(t,x) = x
   */
  def booleanAndTMatcher[X, T : AsBoolean]( eval : (X) => T ) = new BooleanAndTMatcher[X, T](eval)
}
