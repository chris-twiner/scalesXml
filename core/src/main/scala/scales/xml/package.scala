package scales

package object xml extends XmlTypes 
  with XmlParser 
  with XmlPaths
  with XPathMatcher 
  with XmlPrinter 
  with Whitespace 
  with XmlPulls 
  with XmlFactories 
  with TraxSourceConversions
  with XmlUtils
  with PullIteratees
  with equals.XmlEquals {

  import strategies.ElemToken

  val defaultPathOptimisation : PathOptimisationStrategy[QNameToken] = QNameMemoryOptimisation
  val defaultOptimisation : MemoryOptimisationStrategy[QNameToken] = QNameMemoryOptimisation

  val Functions = xpath.Functions

  // work around my own version of SI-5031
  @deprecated(message="Please import Functions._ instead - since 0.3")
  val Attributes = xpath.Attributes
  @deprecated(message="Please import Functions._ instead - since 0.3")
  val Elements = xpath.Elements
  @deprecated(message="Please import Functions._ instead - since 0.3")
  val TextFunctions = xpath.OldTextFunctions
}
