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
  with equals.XmlEquals
  with serializers.SerializingIter
  with xpath.Functions {

  import strategies.ElemToken

  val defaultPathOptimisation : PathOptimisationStrategy[QNameToken] = QNameMemoryOptimisation
  val defaultOptimisation : MemoryOptimisationStrategy[QNameToken] = QNameMemoryOptimisation

  @deprecated(message="Functions - since 0.3 - imports are provided via the xml package object")
  val Functions = new Object()
}
