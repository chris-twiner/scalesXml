package scales.xml

/**
 * All the test*Stream tests trigger convertToStream, the basis of the TrAX support.
 * The other tests are focused on xslt transformations and round trips
 */ 
class AaltoTraxSupportTest extends TraxSupportTest  {

  import scales.utils._
  import ScalesXml._

  /**
   * Miscs also not read :<
   
  def testMiscRoundTrippingTrAX = {
    val testXml = loadXmlReader(resource(this, "/data/MiscTests.xml"), parsers = NoVersionXmlReaderFactoryPool)
    import javax.xml.transform._
    val sr = ScalesResult()
    TransformerFactory.newInstance.newTransformer.transform(testXml, sr)

    MarshallingTest.doMiscTest(sr.doc)
  }*/ 

}
