package scales

package object xml extends dsl.XPathMatcher
  with impl.XmlTypes 
  with impl.Whitespace 
  with impl.XmlFactories 
  with impl.XmlUtils
  with parser.sax.XmlParser 
  with parser.pull.XmlPulls 
  with trax.TraxSourceConversions
  with serializers.XmlPrinter 
  with equals.XmlEquals
  with xpath.XmlPaths
  with xpath.Functions {

  import scales.xml.parser.strategies.{MemoryOptimisationStrategy, PathOptimisationStrategy, QNameMemoryOptimisation, QNameToken}

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

  /**
   * Basis for xmlpulls, an Iterator[PullType]
   */
  type XmlPull = scales.xml.parser.pull.XmlPull

}
