package scales.utils.collection

/**
 * Backwards compat for 2.8.1, its simply Iterator for 2.9.x
 */ 
trait FlatMapIterator[+A] extends Iterator[A] { 
}

trait FlatMapImplicits {
}
