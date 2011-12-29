package scales.utils

/**
 * Similar to the scala DynamicVariable except that StackedThreadLocal is used and therefore remove is performed after the last pop.
 * 
 * Additionally if the value is taken via apply it will assert that the value has been set.
 */
class DynVariable[T](init: T) {
  /** let stl take care of the remove */
  private val tl = StackedThreadLocal[T](init)

  /** Retrieve the current value */
  def apply() = {
    val v = tl.get 
    assert(v.isDefined, "DynVariables must be used within the scope of a withValue call")
    v.get
  }
  
  /**
   * Is there actually any value in this dynamic variable?
   */
  def isDefined() = 
    if (tl.stackSize() == 0) false else true 
  
  
  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value for the variable during execution of thunk
    * @param thunk
    */
  def withValue[S](newval: T)(thunk: =>S): S = {
    tl.push(newval)
  
    try { thunk } 
    finally { tl.pop }
  }

  override def toString: String = "DynVariable(" + () +")"
}

