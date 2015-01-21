package scales.xml.parser.pull.aalto

class AsyncPullTest extends junit.framework.TestCase {

  import junit.framework.Assert._
  import java.io._
  import java.nio.channels._

  import scales.utils._
  import ScalesUtils._
  import scales.xml._
  import ScalesXml._
 
  import scales.xml.impl.NoVersionXmlReaderFactoryPool

  import io._
  import ScalesUtilsIO._

  import Functions._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  import scalaz.EphemeralStream
  import EphemeralStream.emptyEphemeralStream

  import scales.utils.{resource => sresource}

  import scales.aalto.parser.pull._

  val smallBufSize = 10
  
  // Tiny jvm buffer, lots of reloading
  val tinyBuffers = new JVMBufferPool( bufferSize = smallBufSize )

  import serializers._

  import java.nio.charset.Charset
  import scales.utils.{io, resources}
  import resources._ 

  // Just using the parser
  def testRandomAmountsDirectParser = {
    val url = sresource(this, "/data/BaseXmlTest.xml")

    val doc = loadXmlReader(url, parsers = NoVersionXmlReaderFactoryPool)
    val str = asString(doc)

    val stream = url.openStream()
    
    val randomChannelOrig = new RandomChannelStreamWrapper(stream, smallBufSize)
    val randomChannel = randomChannelOrig.wrapped    

    val parser = AsyncParser()

    val empty = () => emptyEphemeralStream

    var nexted = 0
    var headed = 0

    var res = Vector.empty[PullType]
    
    var b : DataChunk = EmptyData
    while(b != EOFData) {
      b = randomChannel.nextChunk
      val s = parser.nextInput(b)
      s(
	el = e => {
	  headed += 1
	  var st = e
	  while(!st.isEmpty) {
	    val h = st.head()
	    res = res :+ h
	    st = st.tail()
	  }
	},
	empty = {nexted += 1;()},
	eof = {nexted += 1;()}
      )
    }
    
    val s = asString(res.iterator : Iterator[PullType])
    assertEquals(s, str)

    assertTrue("we should have more nexted then zeroed - due to boundaries on the available data", randomChannelOrig.zeroed + 1 < nexted)
  }
 

}
