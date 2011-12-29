package scales.utils

object StackedThreadLocal {
  def apply[T]() = new StackedThreadLocal[T](None)
  def apply[T](init: T) = new StackedThreadLocal[T](Some(init))

}

/**
 * Mimics ThreadLocalStorage except that the current tls value is the head of a stack.
 * 
 * Stack items can be pushed and popped to allow tls nesting of values
 */
class StackedThreadLocal[T] private (init: Option[T]) {

  /**
   * If we have a default then we work from the default value not from the empty list.
   */
  private[this] val haveDefault = init.isDefined

  private[this] val tls = new java.lang.ThreadLocal[java.util.ArrayList[T]]() {
    override def initialValue = {
      // create a value regardless == no null checks.
      val c = new java.util.ArrayList[T]()
      if (init.isDefined)
        c.add(init.get.asInstanceOf[T with AnyRef])
      c
    }
  }

  /**
   * 
   * @return the size of the current thread local.
   */
  def stackSize() = {
    var c = tls.get()
    c.size()
  }

  /**
   * Get the current stacked thread local.
   * Use getStackSize() to see if the stack contains
   * items.
   * @return the variable
   */
  def get() = {
    var c = tls.get()
    // can only return null here
    if (c.size() == 0) {
      None
    } else Some(c.get(c.size() - 1))
  }

  /**
   * Removes an item from the stack.  
   * 
   * Note if the tls was given a default value then pushing and popping are still balanced.  If you push 3 times, popping 3 times will remove the tls, regardless of default values.
   * @return true when it the last stack item has been removed
   */
  def pop(): Boolean = {
    val c = tls.get()
    c.remove(c.size() - 1)

    val size = c.size()

    if ((size == 1 && haveDefault) || (size == 0 && !haveDefault)) {
      // really remove it
      tls remove

      true
    } else false
  }

  /**
   * Add an item to the stack
   * @param value
   */
  def push(value: T) {
    var c = tls.get()
    c.add(value)
  }

  /**
   * Override the value
   * 
   * @param value
   */
  def reset(value: T) {
    var c = tls.get()
    if (c == null) {
      c = new java.util.ArrayList[T]()
      c.add(value)
      tls.set(c)
    } else {
      c.set(c.size() - 1, value)
    }
  }

}
