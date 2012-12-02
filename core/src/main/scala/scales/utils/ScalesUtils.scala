package scales.utils

/**
 * Allows the user to extend a user package object with the scales.utils implicits.
 */ 
trait ScalesUtilsImplicits extends collection.path.PathImplicits 
  with IterateeImplicits 
  with collection.IterableUtilsImplicits {
}

/**
 * Allows importing all scales.utils implicits directly
 */ 
object ScalesUtils extends ScalesUtilsImplicits
