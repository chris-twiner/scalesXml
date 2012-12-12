package scales

package object xml extends XmlTypes 
  with parser.sax.XmlParser 
  with xpath.XmlPaths
  with XPathMatcher 
  with XmlPrinter 
  with impl.Whitespace 
  with XmlPulls 
  with impl.XmlFactories 
  with trax.TraxSourceConversions
  with impl.XmlUtils
  with PullIteratees
  with equals.XmlEquals
  with serializers.SerializingIter
  with xpath.Functions {

  import scales.xml.parser.strategies.{QNameMemoryOptimisation, PathOptimisationStrategy, QNameToken, MemoryOptimisationStrategy}

  val defaultPathOptimisation : PathOptimisationStrategy[QNameToken] = QNameMemoryOptimisation
  val defaultOptimisation : MemoryOptimisationStrategy[QNameToken] = QNameMemoryOptimisation

  @deprecated(message="Functions - since 0.3 - imports are provided via the xml package object")
  val Functions = new Object()

  // forwarders

  /**
   * XPath type, provides all XPath Axe against a given collection of XmlPaths 
   * @see [xpath.XPath]
   */ 
  type XPath[PT <: Iterable[XmlPath]] = xpath.XPath[PT]

  /**
   * AttributePath provides access to the parent XmlPath
   */
  type AttributePath = xpath.AttributePath

  /**
   * AttributePaths provides all XPath Axe that are attribute relevant against a given collection of attributes on a colletion of XmlPaths
   * @see [xpath.AttributePaths]
   */ 
  type AttributePaths[PT <: Iterable[XmlPath]] = xpath.AttributePaths[PT]

}
