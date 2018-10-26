package scales.xml.impl

import scales.xml.{Attribute, AttributeQName, XmlPath}

/**
 * exists only to provide Jaxen and JXPath with a document "root"
 */ 
case class DocumentRoot(xmlPath : XmlPath)

/**
 * exists only to provide Jaxen and JXPath with the same document root
 */ 
case class DocsUp[WHAT](what : WHAT, docroot : DocumentRoot)

object ExtraTypesImplicits {
  implicit val toAttrN = (nons: AttributeQName) => Attribute(nons, "")
}
