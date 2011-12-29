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
  with PullIteratees {

  import strategies.ElemToken

  val defaultPathOptimisation : PathOptimisationStrategy[ElemToken] = QNameAndSpeedierStrategy
  val defaultOptimisation : MemoryOptimisationStrategy[ElemToken] = QNameAndSpeedierStrategy

  // work around my own version of SI-5031
  val Attributes = xpath.Attributes
  val Elements = xpath.Elements
  val TextFunctions = xpath.TextFunctions
}
