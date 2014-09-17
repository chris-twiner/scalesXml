package scales.xml

import scales.utils.resources._
import javax.xml.validation._

/**
 * Default XSD SchemaFactory impl
 */ 
object SaxonSchemaFactoryPool extends SimpleUnboundedPool[SchemaFactory] { pool =>
  
  def create = {
    val fac = SchemaFactory.newInstance(javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI)
    fac.setFeature(javax.xml.XMLConstants.FEATURE_SECURE_PROCESSING, false)
    fac
  }

}

/**
 * Test jaxp validation code via the TraxSupport
 */ 
class SaxonValidateTest extends ValidateTest {
 
  // #34 - the schema factory is setting attributes that aren't usable by Saxon on openjdk6 
  def schemaFactory = SaxonXSDSchemaFactoryPool
 
}
