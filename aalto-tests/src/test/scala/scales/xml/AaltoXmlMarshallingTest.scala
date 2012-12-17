package scales.xml

/**
 * _S tests are for the SimpleSerializer
 * 
 */ 
class AaltoXmlMarshallingTest extends XmlMarshallingTest {

  import junit.framework.Assert._

  import java.io._
  import ScalesXml._
  import serializers._
  import scales.utils.resource

  import impl.NoVersionXmlReaderFactoryPool
  import parser.strategies._

  override val readBack_S = { tree : XmlTree => 
    val s = asString(tree)(SimpleSerializerFactory, treeSerializeable)
    loadXmlReader(new StringReader(s), parsers = NoVersionXmlReaderFactoryPool).rootElem 
		  }

  override val readBack_LS = { tree : XmlTree => 
    loadXmlReader(new StringReader(asString(tree)), parsers = NoVersionXmlReaderFactoryPool).rootElem
		  }

  override val readBackDoc_LS = { doc : Doc => 
    loadXmlReader(new StringReader(asString(doc)), parsers = NoVersionXmlReaderFactoryPool)
		  }

  import org.xml.sax.InputSource

  override   def doLoadXml[Token <: OptimisationToken](in : InputSource, strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) = {
    loadXmlReader(in, parsers = NoVersionXmlReaderFactoryPool, strategy = strategy)
  }

  // aalto doesn't support version retreival
  override def test11_prefix : Unit = {
    val tree = doLoadXml(resource(this, "/data/1.1_prefixes.xml"))
    val t2 = tree.copy( prolog = tree.prolog.copy( decl = tree.prolog.decl.copy(version = Xml11)))
    val s = asString(t2)
    assertEquals(test11_val, s)
  }

}
