package scales.xml

/**
 * This package provides string XPath evaluation based on the Jaxen library, sample use: {{{
 * val xpath = ScalesXPath(str, Map(
      "pre" -> "urn:prefix",
      "jh" -> "urn:justHere",
      "def" -> "urn:default"
    ))
 * val result : Iterable[Either[AttributePath, XmlPath]] = xpath.evaluate(x : XmlPath)
 * }}}
 * NB: This class is only available with the scales-jaxen dependencies.
 */ 
package object jaxen {
}
