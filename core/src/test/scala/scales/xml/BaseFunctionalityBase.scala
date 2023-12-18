package scales.xml

// Couldn't resist the name

object BaseTestConstants {
  
  val prefixedPQN = "ns1:{urn:prefix}prefixed"

  val dontRedeclareNoNS = "{}DontRedeclare"

  val shouldRedeclareDefaultNS = "{urn:default}ShouldRedeclare"

//  val expectedKids = List(dontRedeclareNoNS, dontRedeclareNoNS,
//      shouldRedeclareDefaultNS, prefixedPQN)


  type XmlPaths = Iterable[XmlPath]

  import ScalesXml._

  val preNS = Namespace("urn:prefix")
  val pre = preNS.prefixed("pre")
  val jh = Namespace("urn:justHere").prefixed("jh")
  val defo = Namespace("urn:default").prefixed("def")

  val dontRedeclare = NoNamespaceQName("dontRedeclare")
}
