package scales.xml

trait ScalesXmlImplicits extends XmlTypesImplicits 
  with XmlPathImplicits 
  with DslImplicits 
  with QNameImplicits 
  with XmlParserImplicits
  with DefaultXmlVersion
  with XmlPrinterImplicits 
  with TraxConversionImplicits 
  with XmlUtilsImplicits
  with OptimisingStrategiesImplicits
  with serializers.SerializerImplicits
  with xpath.FunctionImplicits
  with PullTypeConversionImplicits 
  with equals.DefaultXmlEquals
  with equals.StreamComparableImplicits 
  with equals.ComparisonContextImplicits
  with equals.FromEqualsImplicit {

}

object ScalesXml extends ScalesXmlImplicits
