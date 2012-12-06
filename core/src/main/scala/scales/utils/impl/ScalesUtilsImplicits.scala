package scales.utils.impl

import scales.utils.{collection, iteratee}

/**
 * Allows the user to extend a user package object with the scales.utils implicits.
 */ 
trait ScalesUtilsImplicits extends collection.path.PathImplicits 
  with iteratee.IterateeImplicits 
  with collection.IterableUtilsImplicits {
}
