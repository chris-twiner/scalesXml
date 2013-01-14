package scales.xml

import ScalesXml.{xmlCBF, fromParserDefault} // note cannot be in parser here

/**
 * Creates DslBuilder instances
 */
object < {
  /**
   * Creates a DslBuilder with qname as the root Elems 
   */ 
  def apply(qname : QName) = dsl.DslBuilder(Elem(qname))
  /**
   * Creates a DslBuilder with the given elem as root
   */ 
  def apply(elem : Elem) = dsl.DslBuilder(elem)
}

/**
 * Creates OptionalDslBuilder instances.  If none of the end Elems contain data (either attributes or child text nodes) the result of the builder is None.
 */
object ?< {
  /**
   * Creates an OptionalDslBuilder with qname as the root Elems 
   */ 
  def apply(qname : QName) = dsl.OptionalDslBuilder(Elem(qname))
  /**
   * Creates a OptionalDslBuilder with the given elem as root
   */ 
  def apply(elem : Elem) = dsl.OptionalDslBuilder(elem)
}
