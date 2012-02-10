package scales.xml

import serializers._

/**
 * _S tests are for the SimpleSerializer
 * 
 */ 
class XmlMarshallingTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._
  import java.io._
      
  import scales.utils._
  import ScalesUtils._
  import ScalesXml._

  import Functions._

  val ns = Namespace("urn:test:uri")
  val nsa = Namespace("urn:test:uri:attribs")
  val nsp = nsa.prefixed("pre")

  val readBack_S = { tree : XmlTree => 
    loadXml(new StringReader(asString(tree)(SimpleSerializerFactory, treeSerializeable))).rootElem 
		  }

  val readBack_LS = { tree : XmlTree => 
    loadXml(new StringReader(asString(tree))).rootElem
		  }

  val readBackDoc_LS = { doc : Doc => 
    loadXml(new StringReader(asString(doc)))
		  }

  val readBackDoc_LSP = { doc : Doc => 
    pullXmlCompletely(new StringReader(asString(doc)))
		  }

  def catchAll[T]( it : => T ) : T = {
    try{
      it
    } catch {
      case t : Throwable => println("oops : " + t.getMessage);t.printStackTrace; throw t
    }
  }

  def doTestAttributesNoPrefixTest( readBack : XmlTree => XmlTree ) = catchAll{
    val builder = <(ns{"Elem"})
    val withAttribs = builder /@ (nsa("pre","attr1") -> "val1", "attr2" -> "val2", nsp{"attr3"} -> "val3")
    //println(dumpTree(withAttribs))
    val marshalled = readBack(withAttribs)
    assertTrue("Should have had attr1",marshalled.section.attributes.contains(nsa("pre","attr1") : QName))
    assertTrue("Should have had attr2",marshalled.section.attributes.contains(NoNamespaceQName("attr2") : QName))
  }

  def testAttributesNoPrefixNamespace_S = 
    doTestAttributesNoPrefixTest(readBack_S)
  
  def doTestElemsDefaultNS( readBack : XmlTree => XmlTree ) = catchAll{
    val builder = ns("Root") /( ("Child"l) / ns("Grand")  ) 
//    printTree(builder)
    val marshalled = readBack(builder)
    //println(dumpTree(marshalled))
    assertTrue( "Should have had the path match " , top(marshalled).\*("Child").\*(ns("Grand")).size == 1  )
  }
  
  def doValueTest( readBack : XmlTree => XmlTree ) = catchAll{
    val builder = ns("Root") /( (("Child"l) /@("attr" -> "\"in\"") ) /(ns("Grand")~>"A Value")  ) 
    //printTree(builder)
    val marshalled = readBack(builder)
    //printTree(marshalled)
    assertEquals( "A Value" , text(top(marshalled)))
  }

  def testElemsDefaultNS_S = 
    doTestElemsDefaultNS(readBack_S)

  def testAttributesNoPrefixNamespace_LS = 
    doTestAttributesNoPrefixTest(readBack_LS)

  def testElemsDefaultNS_LS = 
    doTestElemsDefaultNS(readBack_LS)

  def testValue_S = doValueTest(readBack_S)

  def testValue_LS = doValueTest(readBack_LS)

  def testInvalidElementEncoding = {
    val builder = ns("Rööt") /( (("Child"l) /@("attr" -> "\"in\"") ) /(ns("Grand")~>"A Value")  ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)
    assertTrue("Did not throw an error",r.isDefined)
    assertTrue("Should have been a ",r.get.isInstanceOf[InvalidCharacterInMarkup])
    assertEquals("Rööt",r.get.getMessage())
  }

  def testInvalidAttrEncoding = {
    val builder = ns("Root") /( (("Child"l) /@("ättr" -> "\"in\"") ) /(ns("Grand")~>"A Value")  ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)
    assertTrue("Did not throw an error",r.isDefined)
    assertTrue("Should have been an InvalidCharacterInMarkup",r.get.isInstanceOf[InvalidCharacterInMarkup])
    assertEquals("ättr",r.get.getMessage())
  }

  def testAttrEncoding = {
    val builder = ns("Root") /( (("Child"l) /@("attr" -> "ächte") ) /(ns("Grand")~>"A Value")  ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)
    assertFalse("Should not throw an error",r.isDefined)
    //println(str.toString)
    
    assertTrue("Should have been encoded for US-ASCII", (str.toString.indexOf("&#228;chte") > -1) || (str.toString.indexOf("&#xe4;chte") > -1)) //&#228;  &#xe4; the same. xe4 for jre non hex for xalan
  }

  def testCDataEncoding = {//cdata isn't written out correctly
    val builder = ns("Root") /( "Child"l ) /( CData("ächte") ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)
    //println(str.toString)

    assertTrue("Did not throw an error",r.isDefined)
    assertTrue("Should have been an CDataCannotBeEncoded",r.get.isInstanceOf[CDataCannotBeEncoded])
    assertEquals("ächte",r.get.getMessage())
  }
  
  def testIncompatibleQNameVersions = {
    val builder = ns("Root") /( "Child\u10000"l(Xml11,fromParserDefault) ) //tests the implmementation but I don't get the 10000 from the spec, both 1.0 and 1.1 include that
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)
//    println(str.toString)

    assertTrue("Did not throw an error",r.isDefined)
    assertTrue("Should have been an Incompatible",r.get.isInstanceOf[IncompatibleQNameVersions])
    assertEquals("Child\u10000",r.get.getMessage())
  }

  def testCommentEscaping = {//comments aren't escaped with character references
    val builder = ns("Root") /( "Child"l ) /( Comment("ächte") ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str, encoding = US_ASCII)))(builder)

    assertTrue("Did not throw an error",r.isDefined)
    assertTrue("Should have been a CommentCannotBeEncoded",r.get.isInstanceOf[CommentCannotBeEncoded])
    assertEquals("ächte",r.get.getMessage())
  }
//TODO some positive tests

  def testCDataEnd = {//cdata isn't written out correctly
    val builder = ns("Root") /( "Child"l ) /( CData("ächte &") ) 
    val str = new java.io.StringWriter()

    val r = foldPrint(XmlOutput(SerializerData(str)))(builder)
    //println(str.toString)

    assertFalse("Should not throw an error",r.isDefined)
    assertTrue("Should have had ächte &]]>",str.toString.indexOf("ächte &]]>") > -1)
  }
  
  val miscml = resource(this, "/data/MiscTests.xml")

  /**
   * Reads the MiscTests to a doc, and writes the doc back out again, then re-reads it, should pass the same tests
   */ 
  def testMiscRoundTripping = {
    val testXml = loadXml(miscml)
    MarshallingTest.doMiscTest(testXml)
//    printTree(testXml)
    val r = readBackDoc_LS(testXml)
    MarshallingTest.doMiscTest(r)
  }

  def testMiscRoundTrippingPull = {
    val testXml = pullXmlCompletely(miscml)
    MarshallingTest.doMiscTest(testXml)
//    printTree(testXml)
    val r = readBackDoc_LSP(testXml)
    MarshallingTest.doMiscTest(r)
  }

  def testPullMiscRountTripping = {
    var pull = pullXml(miscml)
    //printTree(pull)
    // serialize the stream is enough to read it all
    asString(pull)
    MarshallingTest.doMiscTest(pull)

    pull = pullXml(miscml)
    val r = loadXml(new StringReader(asString(pull)))
    MarshallingTest.doMiscTest(pull)
    MarshallingTest.doMiscTest(r)

    // perhaps unnecessary but why not
    pull = pullXml(miscml)
    val r2 = pullXml(new StringReader(asString(pull)))
    asString(r2)
    MarshallingTest.doMiscTest(r2)
    MarshallingTest.doMiscTest(pull)

  }
  
  def testPullMarshalling : Unit = {
    var pull = pullXml(miscml)
    //printTree(pull)
    // serialize the stream is enough to read it all
    asString(pull)
  }
  
  val test11_val = """<?xml version="1.1" encoding="US-ASCII"?><x xmlns:n1="http://www.w3.org"><n1:a/><x xmlns:n1=""><x xmlns:n1="http://www.w3.org"><n1:a/></x></x></x>"""

  def test11_prefix : Unit = {
    val tree = loadXml(resource(this, "/data/1.1_prefixes.xml"))
    val s = asString(tree)
    assertEquals(test11_val, s)
  }

  def createIt(implicit defaultVersion : XmlVersion) {
    val ns = Namespace("http://www.w3.org")
    val pre = ns.prefixed("n1")
    val disabled = Elem("x"l, Namespace("").prefixed("n1"))
    val a = Elem(pre("a"))
    val x = Elem("x"l, pre)

    val tree = x /( a, disabled /( x /( a )))
    val doc = Doc(tree, Prolog(Declaration(Xml11, java.nio.charset.Charset.forName("US-ASCII"))))

    val s = asString(doc)
    assertEquals(test11_val, s)
  }

  def testTo11 : Unit = {
    createIt(Xml11)
  }

  def testTo11with10 : Unit = {
    try{
      createIt(Xml10)
    } catch {
      case t : Throwable if (t.getMessage.contains("''")) =>
	()
      case t : Throwable => throw t
    }
  }

/*
 XML 1.1 does not work with pull parsers :<
  def test11_prefix_pull : Unit = {
    val tree = pullXmlCompletely(resource(this, "/data/1.1_prefixes.xml"))
    val s = asString(tree)
    assertEquals(test11_val, s)
  }
*/

}

object MarshallingTest {
  import junit.framework.Assert._

  def doMiscTest( doc : DocLike ) {
    def doCommentsTest( coms : Seq[XmlItem] ) {
      for( i <- 0 to 3 ){
       assertEquals(" useless comment "+i+" ", coms(i).value)
      }
    }
    def doPIsTest( pis : Seq[XmlItem] ) {
      for( i <- 0 to 3 ){
       val pi = pis(i).asInstanceOf[PI]
       assertEquals("forTest"+i+" "+i, pi.target + " " + pi.value)
      }
    }
    assertEquals(8, doc.prolog.misc.size)

    val (ppis, pcomments) = doc.prolog.misc.map(_.fold[XmlItem](x=>x,y=>y)).splitAt(4)
    doPIsTest(ppis)
    doCommentsTest(pcomments)

    assertEquals(8, doc.end.misc.size)
    val (ecomments, epis) = doc.end.misc.map(_.fold[XmlItem](x=>x,y=>y)).splitAt(4)
    doPIsTest(epis)
    doCommentsTest(ecomments)

  }

}
