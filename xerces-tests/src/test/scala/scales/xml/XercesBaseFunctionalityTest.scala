package scales.xml

import scales.utils.TestUtils._

import ScalesXml._

class XercesBaseFunctionalityTest extends test.BaseFunctionalityTest {

// xerces doesn't read the text here
  override def testTextPosIsLast = {
    val expected = List("prefixed text",
			"",
			"") //last bit at the end of the doc after the commment
			   


    assertCompare(expected,
      textPosIsLast
      ) { value(_).trim }
  }

}
