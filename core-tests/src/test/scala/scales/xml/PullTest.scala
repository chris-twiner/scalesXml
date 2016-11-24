package scales.xml

class PullTest extends junit.framework.TestCase {

  import ScalesXml._
  import junit.framework.Assert._
  import scales.utils._
  import io._
  import resources._

  val Default = Namespace("urn:default")
  val DefaultRoot = Default("Default")

  
  import scales.utils.{resource => sresource}
 
  /*
   * pump up number when cold, but make sure its even.
   *
   * 5000000 takes around 185s (as of 14.01.2010) and shows no leaking/unneccesary retention.
   */
  val maxIterations = 500//0000 

  def testSimpleLoad = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", pull.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)
    
    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok 
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    pull.close
    assertTrue("Should have been closed", pull.isClosed)
  }

  def testResourceLoad = {
    val (resource, pull) = pullXmlResource(sresource(this, "/data/BaseXmlTest.xml"))
    assertTrue("didn't have any events",pull.hasNext)
    assertFalse("should not have been closed", resource.isClosed)
    var next = pull.next
    assertTrue("should have been left", next.isLeft)
    
    next.left.get match {
      case Elem(DefaultRoot,_,_) => () // is ok 
      case a @ _ => fail("Should have been {urn:default}Default was "+a)
    }

    resource.closeResource
    assertTrue("Should have been closed", resource.isClosed)
  }

  def testProxiedCloser = {
    val strr = new java.io.StringReader("My String")
    val proxied = new ProxiedCloseOnNeedReader(strr)

    proxied.close() // should be a noop
    assertFalse("Was closed for some reason", proxied.isClosed)

    var arr = Array[Char](10)

    val count = proxied.read(arr)
    assertEquals("My String".substring(0,count),arr.mkString(""))

    proxied.closeResource
    assertTrue("Should have been closed", proxied.isClosed)  
  }

  def testChainedCloseOnNeed = {
    import java.io.StringReader
    def getOne = new StringReader("My String") with CloseOnNeed {
      def doClose() {
	super[StringReader].close()
      }
    }
    val str1 = getOne
    val str2 = getOne
    val str3 = getOne
    
    val joined = str1 ++ str2 ++ str3
    assertFalse("1 Should not have been closed", str1.isClosed)  
    assertFalse("2 Should not have been closed", str2.isClosed)  
    assertFalse("3 Should not have been closed", str3.isClosed)  
    assertFalse("Joined Should not have been closed", joined.isClosed)  

    joined.closeResource

    assertTrue("1 Should have been closed", str1.isClosed)  
    assertTrue("2 Should have been closed", str2.isClosed)  
    assertTrue("3 Should have been closed", str3.isClosed)  
    assertTrue("Joined Should have been closed", joined.isClosed)    
  }

  def testDump = {
    val pull = pullXml(sresource(this, "/data/BaseXmlTest.xml"))

    def out(it : String) : Unit = 
      ()//it
    
    for{event <- pull}{
      event match {
	case Left(x) => x match {
	  case Elem(qname, attrs, ns) =>
	    out("<" + qname + attrs.map( x => " "+x.name +"='"+x.value+"'" ).mkString(" ") + ">")

	  case item : XmlItem =>
	    out("item "+item)
	} 
	case Right(EndElem(qname, ns)) =>
	  out("</"+ qname +">")
	
      }
    }
    
    assertTrue("Should have been closed", pull.isClosed)
  }

/*
  def testEphemeral = {
    def ntimes( i : Int, max : Int) : EphemeralStream[Int] =
      if (i < max) 
	EphemeralStream[Int](i) ++ (ntimes( i + 1, max))
      else
	EphemeralStream.empty

    val itr = ntimes(0, 300000).iterator
    println(itr.next)
  }*/

  type FiveStrings = (String,String,String,String,String)

  val expectedHead = ("264","chris.twiner","dir","M","/trunk/scalesUtils")

  def testIterator = {
    val pull = pullXml(sresource(this, "/data/svnLogIteratorEg.xml"))
    val LogEntries = List("log"l,"logentry"l)

    var i = 0

    val it = scales.xml.iterate(LogEntries, pull.it)
    val bits = for{ entry : XmlPath <- it
	revision <- entry.\.*@("revision"l).one
	author <- entry.\*("author"l).one
	path <- entry.\*("paths"l).|>{x=> i+=1;x}.\*("path"l)
	kind <- path.\.*@("kind"l)
	action <- path.\.*@("action"l)
    } yield (text(revision), value(author), text(kind), text(action), value(path))

    val t = bits.next//iterator.next
    //println( t._1+ " " + t._2 + " " + t._3 + " " + t._4 + " " + t._5 )
    assertEquals(1, i)
    assertEquals(expectedHead, t)

    // sanity check
    if (it.isTraversableAgain) assertTrue(it.nonEmpty)

    pull.close
  }

  def testIteratorForStrictSelectivePulling() = {
    val expectedPlants = List(
      ("English ivy","Hedera helix", "3", "Mostly Shady", "$9.99", "000100"),
      ("Dwarf periwinkle","Vinca minor", "3", "Mostly Shady", "$12.10", "000409")
    )

    val pull = pullXml(
      sresource(this, "/data/plant_catalog.xml"),
      strictPath = List(
        NoNamespaceQName("CATALOG"),
        NoNamespaceQName("PLANT")
      )
    )
    val PlantEntries = List("CATALOG"l, "PLANT"l)

    val it = scales.xml.iterate(PlantEntries, pull.it)
    val plantsIt = for {
      entry: XmlPath <- it
      common <- entry.\*("COMMON"l).one
      botanical <- entry.\*("BOTANICAL"l).one
      zone <- entry.\*("ZONE"l).one
      light <- entry.\*("LIGHT"l).one
      price <- entry.\*("PRICE"l).one
      availability <- entry.\*("AVAILABILITY"l).one
    } yield (value(common), value(botanical), value(zone), value(light), value(price), value(availability))

    val plants = plantsIt.toList

    assertEquals(expectedPlants.size, plants.size)
    for ((plant, idx) <- plants.zipWithIndex)
      assertEquals(expectedPlants(idx), plant)

    // sanity check
    if (it.isTraversableAgain) assertTrue(it.nonEmpty)

    pull.close
  }
}