package scales.xml

/**
 * All the test*Stream tests trigger convertToStream, the basis of the TrAX support.
 * The other tests are focused on xslt transformations and round trips
 */ 
class AaltoTraxSupportTest extends TraxSupportTest  {

  import scales.utils._
  import ScalesXml._

  /**
   * Same as the marshalling round trip but uses TrAX to transform
   */ 
  def testMiscRoundTrippingTrAX = {
    val testXml = loadXml(resource(this, "/data/MiscTests.xml"))
    import javax.xml.transform._
    val sr = ScalesResult()
    TransformerFactory.newInstance.newTransformer.transform(testXml, sr)

    MarshallingTest.doMiscTest(sr.doc)
  }

}
