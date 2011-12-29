package scales.xml

/**
 * Test jaxp validation code via the TraxSupport
 */ 
class ValidateTest extends junit.framework.TestCase {

  import junit.framework.Assert._

  import scales.utils._
  import ScalesUtils._

  import TestUtils._

  import ScalesXml._

  def testSimpleValidation = {
    val xsd = loadXml(resource(this,"/data/personal.xsd"))
    val schema = newSchema(xsd)

    val doc = loadXml(resource(this, "/data/personal-schema.xml"))
    schema.newValidator.validate(doc)
  }

}
