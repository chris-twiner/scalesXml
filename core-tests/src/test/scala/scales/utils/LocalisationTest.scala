package scales.utils

class LocalisationTest extends junit.framework.TestCase {
  import ScalesUtils._

  import junit.framework.Assert._;
  
  import java.util.Locale

  val default = Locale.getDefault
  val en = Locale.UK

  override def setUp = {
    Locale.setDefault(en)
  }

  override def tearDown = {
    Locale.setDefault(default)
  }

  def testSimple = {
    val res = Resource()(classOf[LocalisationTest])
    
    assertEquals("SimpleString", res("simple"))
  }

  def testFillerNonId = {
    val res = Resource()(this)
    
    assertEquals("Filler 1 with 2 and 3", res.get("filler", 1, 2, 3, 4))
    assertEquals("Filler 1 with 2 and XXX", res("filler", 1, 2))
  }

  def testSimpleDE = {
    val res = Resource( () => new java.util.Locale("de"))(classOf[LocalisationTest])
    
    assertEquals("EinfachString", res("simple"))
  }

  def testFillerNonIdDE = {
    val res = Resource( () => new java.util.Locale("de"))(classOf[LocalisationTest])
    
    assertEquals("Füllung 1 mit 2 und 3", res.get('filler, 1, 2, 3, 4))
    assertEquals("Füllung 1 mit 2 und XXX", res("filler", 1, 2))
  }

  def testSimpleId = {
    val res = Resource()(classOf[LocalisationTest])
    
    assertEquals("[simple] SimpleString", res.getWithId("simple"))
  }

  def testFillerId = {
    val res = Resource()(this)
    
    assertEquals("[filler] Filler 1 with 2 and 3", res.getWithId("filler", 1, 2, 3, 4))
    assertEquals("[filler] Filler 1 with 2 and XXX", res(true, "filler", 1, 2))
  }
}
