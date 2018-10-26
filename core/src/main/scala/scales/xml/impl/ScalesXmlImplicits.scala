package scales.xml.impl

import scales.xml.{dsl, equals, parser, serializers, trax, xpath}

trait ScalesXmlImplicits extends XmlTypesImplicits 
  with DefaultXmlVersion
  with QNameImplicits 
  with XmlUtilsImplicits
  with dsl.DslImplicits
  with dsl.OptionalDslBuilderImplicits
  with parser.sax.XmlParserImplicits
  with parser.strategies.OptimisingStrategiesImplicits
  with trax.TraxConversionImplicits 
  with serializers.SerializerImplicits
  with serializers.XmlPrinterImplicits 
  with xpath.FunctionImplicits
  with xpath.XmlPathImplicits 
  with trax.PullTypeConversionImplicits 
  with equals.DefaultXmlEquals
  with equals.StreamComparableImplicits 
  with equals.ComparisonContextImplicits
  with equals.FromEqualsImplicit {

}

