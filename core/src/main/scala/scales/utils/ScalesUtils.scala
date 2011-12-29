package scales.utils

/**
 * Allows the user to extend a user package object with the scales.utils implicits.
 */ 
trait ScalesUtilsImplicits extends PathImplicits 
  with LocalisedImplicits with IterateeImplicits 
  with IterableUtilsImplicits {
}

/**
 * Allows importing all scales.utils implicits directly
 */ 
object ScalesUtils extends ScalesUtilsImplicits
