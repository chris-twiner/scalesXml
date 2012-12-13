package scales.xml.impl

import scales.xml.{xpath, trax, equals, parser, serializers, XmlPrinterImplicits}

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
  with trax.PullTypeConversionImplicits 
  with equals.DefaultXmlEquals
  with equals.StreamComparableImplicits 
  with equals.ComparisonContextImplicits
  with equals.FromEqualsImplicit {

}

