package scales.xml

/**
 * All the test*Stream tests trigger convertToStream, the basis of the TrAX support.
 * The other tests are focused on xslt transformations and round trips
 */ 
class TraxSupportTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._

  import scales.utils._
  import ScalesUtils._

  import TestUtils._

  import ScalesXml._
  
  val ns = Namespace("uri:test") 

  val elem = ns("Elem")
  val child = ns("Child")
  val childl = "Child"l
  val root = ns("Root")
  val child1 = ns("Child1")
  val child2 = ns("Child2")
  val child3 = ns("Child3")
  val fred = ns("fred")

  val texts = elem / (<(child)) / (Text("some text"), Text("Some more text"), Text("yet more"))
    
  val oneChild = root /( childl )

  val manyChildren = <(elem) / (child, child2, child) /( <(fred) ) /(Text("some text"))

  val manyChildrenDeep = <(elem) /(child1 /( child2 /(  child3 ) ))

  val manyChildrenDeepNested = <(elem) /(
    child1 /( child2 /(  child3 ) ), 
    child1 /( child2 /(  child3 ) ))

  val mixed = <(elem) /(Text("in front")) /(child) /( Text("In Back"))

  def doStreamTest(in : XmlTree, against : List[PullType]) {
    compare(convertToStream(in).toIterable, against)(x=>x)
  }

  def testTextsStream = {
    doStreamTest(texts, List(
      Left(Elem(elem)),
      Left(Elem(child)),
      Right(EndElem(child)),
      Left(Text("some text")),
      Left(Text("Some more text")),
      Left(Text("yet more")),
      Right(EndElem(elem,Map()))
     ))
  }
  
  def testOneChildStream = {
    doStreamTest(oneChild, List(
      Left(Elem(root)),
      Left(Elem(childl)),
      Right(EndElem(childl)),
      Right(EndElem(root))
      ))
  }

  def testManyChildrenStream = {
    doStreamTest(manyChildren, List(
      Left(Elem(elem)),
      Left(Elem(child)),
      Right(EndElem(child)),
      Left(Elem(child2)),
      Right(EndElem(child2)),
      Left(Elem(child)),
      Right(EndElem(child)),
      Left(Elem(fred)),
      Right(EndElem(fred)),
      Left(Text("some text")),
      Right(EndElem(elem))
      ))
  }

  def testManyChildrenDeepStream = {
    doStreamTest(manyChildrenDeep, List(
      Left(Elem(elem)),
      Left(Elem(child1)),
      Left(Elem(child2)),
      Left(Elem(child3)),
      Right(EndElem(child3)),
      Right(EndElem(child2)),
      Right(EndElem(child1)),
      Right(EndElem(elem))
    ))
  }

  def testManyChildrenDeepNestedStream = {
    doStreamTest(manyChildrenDeepNested, List(
      Left(Elem(elem)),
      Left(Elem(child1)),
      Left(Elem(child2)),
      Left(Elem(child3)),
      Right(EndElem(child3)),
      Right(EndElem(child2)),
      Right(EndElem(child1)),
      Left(Elem(child1)),
      Left(Elem(child2)),
      Left(Elem(child3)),
      Right(EndElem(child3)),
      Right(EndElem(child2)),
      Right(EndElem(child1)),
      Right(EndElem(elem))
    ))
  }

  def testMixedStream = {
    doStreamTest(mixed, List(
      Left(Elem(elem)),
      Left(Text("in front")),
      Left(Elem(child)),
      Right(EndElem(child)),
      Left(Text("In Back")),
      Right(EndElem(elem))
    ))
  }

  /**
   * Same as the marshalling round trip but uses TrAX to transform
   * NOTE the misc items are ignored by the 1.6 jdk jaxp impl 0 found :<
   * the test is moved to Saxon Tests, where they seem to care about such things.
  def testMiscRoundTrippingTrAX = {
    val testXml = loadXml(resource(this, "/data/MiscTests.xml"))
    import javax.xml.transform._
    val sr = ScalesResult()
    TransformerFactory.newInstance.newTransformer.transform(testXml, sr)

    MarshallingTest.doMiscTest(sr.doc)
  }*/
  
}
