package scales.xml

trait ScalesXmlImplicits extends XmlTypesImplicits 
  with xpath.XmlPathImplicits 
  with DslImplicits 
  with QNameImplicits 
  with parser.sax.XmlParserImplicits
  with DefaultXmlVersion
  with XmlPrinterImplicits 
  with trax.TraxConversionImplicits 
  with XmlUtilsImplicits
  with parser.strategies.OptimisingStrategiesImplicits
  with serializers.SerializerImplicits
  with xpath.FunctionImplicits
  with PullTypeConversionImplicits 
  with equals.DefaultXmlEquals
  with equals.StreamComparableImplicits 
  with equals.ComparisonContextImplicits
  with equals.FromEqualsImplicit {

}

object ScalesXml extends ScalesXmlImplicits
