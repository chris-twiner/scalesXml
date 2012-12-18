package scales.xml

import ScalesXml.{xmlCBF, fromParserDefault} // note cannot be in parser here

object < {
  def apply(qname : QName) = dsl.DslBuilder(Elem(qname))
  def apply(elem : Elem) = dsl.DslBuilder(elem)
}
