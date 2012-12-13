package scales.xml

import ScalesXml.{xmlCBF, fromParserDefault} // note cannot be in parser here

object < {
  def apply(qname : QName) = impl.DslBuilder(Elem(qname))
  def apply(elem : Elem) = impl.DslBuilder(elem)
}
