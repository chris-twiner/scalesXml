package scales.xml

import serializers._

/**
 * Equality tests agaisnt QNameTree and friends
 * 
 */ 
class XmlMarshallingTest29 extends junit.framework.TestCase {
  
  import junit.framework.Assert._
  import java.io._
      
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  import Functions._

  val xmlFile = resource(this, "/data/BaseXmlTest.xml")

  import scales.xml.equals._
  import scalaz._
  import Scalaz._

  def testQNameTreeRoundTripping : Unit = {
    val xorig = loadXml(xmlFile)
    
    val x = loadXml(xmlFile, strategy = strategies.QNameTreeOptimisation)
    //println(asString(x))

    assertTrue("weren't identical ", x === xorig)
    
    val s = asString(x)
    val nx = loadXml(new StringReader(s), strategy = strategies.QNameTreeOptimisation)

    assertTrue("roundtrip not identical", x === nx )
  }


  def testQNameElemTreeRoundTripping : Unit = {
    val xorig = loadXml(xmlFile)
    
    val x = loadXml(xmlFile, strategy = strategies.QNameElemTreeOptimisation)
    //println(asString(x))

    assertTrue("weren't identical ", x === xorig)
    
    val s = asString(x)
    val nx = loadXml(new StringReader(s), strategy = strategies.QNameElemTreeOptimisation)

    assertTrue("roundtrip not identical", x === nx )
  }



}
