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

  import parser.strategies._

  import org.xml.sax.InputSource
  import ScalesXml._
  import Functions._

  def schemaFactory: resources.Loaner[javax.xml.validation.SchemaFactory] = scales.xml.impl.DefaultXSDSchemaFactoryPool
 
  def doLoadXml[Token <: OptimisationToken](in : InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXml(in, strategy = strategy)
  }

  def testSimpleValidation = {
    val xsd = doLoadXml(resource(this,"/data/personal.xsd"))
    val schema = newSchema(xsd)

    val doc = doLoadXml(resource(this, "/data/personal-schema.xml"))
    schema.newValidator.validate(doc)
  }

}
