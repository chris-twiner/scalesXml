package scales.utils.collection

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue

/**
 * Only created once and via calcOnce
 */ 
sealed trait Once[T] {
  val value : T
}

/**
 * Simple helper functions to get and remove ConcurrentLinkedQueues from a ConcurrentHashMap
 */
trait ConcurrentMapUtils {

  def getList[K, T](key: K, mapToList: ConcurrentHashMap[K, ConcurrentLinkedQueue[T]]): ConcurrentLinkedQueue[T] = 
    valueOf(key, mapToList)(
      new ConcurrentLinkedQueue[T]())

  def removeList[K, T](key: K, mapToList: ConcurrentHashMap[K, ConcurrentLinkedQueue[T]]): ConcurrentLinkedQueue[T] = 
    removeOr(key,mapToList)(new ConcurrentLinkedQueue[T]())

  /**
   * Removes the value, returning either it or a new item (stops end code worrying about nulls etc..
   */ 
  def removeOr[K, T](key: K, map: ConcurrentHashMap[K, T])(newT : => T): T = {
    var res = map.remove(key)
    if (res == null) {
      res = newT
    }

    res
  }

  /**
   * Calculates the value once and only once (using a lazy val), returning the result, use this approach for expensive calculations.
   */
  def calcOnce[K,T](key : K, map : ConcurrentHashMap[K, Once[T]])( calc : => T ) : T = 
    valueOf(key, map)(new Once[T]{
	lazy val value = calc
      }).
    // doesn't matter which thread did this
    value
  
  /**
   * retrieves the value of a concurrent hashmap against a given key, creating if necessary.  Note it makes no gaurantee of once only semantics for the value generation
   */
  def valueOf[K,T](key : K, map : ConcurrentHashMap[K, T])( newT : => T ) : T = {
    var value = map.get(key)
    if (value == null) {
      value = newT
      val res = map.putIfAbsent(key, value)
      value = if (res == null) value else res
    }

    value
  }
}
