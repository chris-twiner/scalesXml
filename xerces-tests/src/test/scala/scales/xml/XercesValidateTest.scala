package scales.xml

import scales.utils.resources._
import javax.xml.validation._
import ScalesXml._

/**
 * Test jaxp validation code via the TraxSupport
 */ 
class XercesValidateTest extends ValidateTest {
 
  /* #34 - the schema factory is setting attributes that aren't usable by Saxon on openjdk6
   * due to the StAXValidtor being chosen.  Wierdly although the traxSourceShouldSerialize
   * check isn't being hit for openjdk6 the rest of it behaves, just not this wierd corner. 
  override def doValidate(doc: scales.xml.Doc, schema: javax.xml.validation.Schema) = 
    schema.newValidator.validate(asStreamSource(doc)) // #35 - the workaround
  */
  
}
