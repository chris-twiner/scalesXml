package delme

object ReplaceStuff {

  val userF = ()=>{ 
    var env = System.getenv("USERNAME")
    if (env eq null)
      env = System.getenv("USER")
    if (env eq null)
      "Unknown User"
    else
      env
  }

  val sysEnv = {
//    import scala.collection.jcl.Conversions._
    import scala.collection.JavaConverters._
    System.getenv().asScala.map{(x) => (x._1, () => x._2)}
  }
  val sysProperties = {
//    import scala.collection.jcl.Conversions._
    import scala.collection.JavaConverters._
    System.getProperties().asScala.map{(x) => (x._1.toString, () => x._2.toString)}
  }

  def siteTokens = Map( "User" -> userF, "timestamp" -> { () => {new java.util.Date().toString}}, 
		   "datetime" -> {() => {java.text.DateFormat.getDateTimeInstance.format(new java.util.Date())}},
		   "artifactID" -> {() => "ffff"} ) ++ sysEnv ++ sysProperties

  
  def hasStart(in : String, pos : Int ) : (Int, Boolean) = {
    val npos = in.indexOf("${", pos)
      if (npos > -1) {
	// was it escaped?
	if (npos == 0 || ( npos > 0 && in.charAt(npos -1) != '\\') ){
	  (npos, true)
	} else (npos, false)
      } else (-1, false)
  }

  /**
   * Like the Ant properties system the tokens may have tokens within their values, this expands them
   * allowing for all kinds of crazy meta-property action.
   */ 
  def expand( tokens : Map[String, String] ) : Map[String, String] = {
    @scala.annotation.tailrec
    def iExpand( tokens : Map[String, String] ) : Map[String, String] = {
      var hasFound = false
      val res = tokens.map{ x => 
	val (npos, hasEnd) = hasStart(x._2, 0)
	if (hasEnd) {
	  hasFound = true
	  (x._1, replaceTokens(x._2, tokens, false))
	} else x
      }
      if (hasFound) {
	iExpand(Map[String, String]() ++ res)
      } else Map[String, String]() ++ res
    }
    iExpand(tokens)
  }

  def replaceTokens( in : String, tokens : Map[String, String] ) : String = 
    replaceTokens( in, tokens, true )

  def replaceTokens( in : String, tokens : Map[String, String], shouldExpand : Boolean ) : String = {
    val buf = new StringBuffer

    val itokens = if (shouldExpand) expand(tokens) else tokens

    def iReplace( pos : Int ) : String = {
/*     val npos = in.indexOf("${", pos)
      if (npos > -1) {
	// was it escaped?
	if (npos == 0 || ( npos > 0 && in.charAt(npos -1) != '\\') ){
*/
      val (npos, hasEnd) = hasStart(in, pos)
      if (npos > -1) {
	if (hasEnd) {
	  if (pos != npos)
	    buf.append(in.substring(pos, npos))
	
	  val epos = in.indexOf("}", npos)
	  if (epos == -1) buf.append(in.substring(npos+2)).toString
	  else {
	    val name = in.substring( npos + 2, epos )
	    buf.append( itokens.get(name).getOrElse("Unknown"))
	    iReplace(epos + 1)
	  }
	} else {
	  buf.append(in.substring(pos, npos - 1)) // remove the backslash
	  buf.append("${")
	  iReplace(npos + 2) // jump past the ${
	}
      } else {
	buf.append(in.substring(pos))
	buf.toString
      }
    }
    iReplace( 0 )
  }

/*
  def replaceTokens( in : String, tokens : Map[String, String] ) : String = {
    val buf = new StringBuffer
    @scala.annotation.tailrec
    def iReplace( pos : Int ) : String = {
      val npos = in.indexOf("${", pos)
      if (npos > -1) {
	// was it escaped?
	if (npos == 0 || ( npos > 0 && in.charAt(npos -1) != '\\') ){
//	  println("npos "+npos+" pos "+ pos)
	  if (pos != npos)
	    buf.append(in.substring(pos, npos))
	  // skip it  
	  //iReplace(npos + 2, true)
	
	  val epos = in.indexOf("}", npos)
	  if (epos == -1) buf.append(in.substring(npos+2)).toString
	  else {	    
	    val name = in.substring( npos + 2, epos )
//	    println("found name " + name)
	    buf.append( tokens.get(name).getOrElse("Unknown"))
	    iReplace(epos + 1)
	  }
	} else {
	  //println(
	  buf.append(in.substring(pos, npos - 1)) // remove the backslash
	  buf.append("${")
	  iReplace(npos + 2) // jump past the \${
	}
      } else {
//	println(" npos was -1 pos "+ pos)
	// annoying special case for beginning of string
	//if (
	buf.append(in.substring(pos))
	buf.toString
      }
    }
    iReplace( 0 )
  }
  */
  implicit def evalVals( orig : Map[String, ()=>String] ) : Map[String, String] = orig.map((x) => (x._1,x._2()))
}

class ReplaceTokensTest extends junit.framework.TestCase {
  import ReplaceStuff._
  import junit.framework.Assert._
  
  def testSimple = 
    assertEquals("fred",replaceTokens("fred",siteTokens)) 

  def testEmpty = 
    assertEquals("",replaceTokens("",siteTokens))

  def testStarts = 
    assertEquals("",replaceTokens("${",siteTokens))

  def testEmptyUnknown = 
    assertEquals("Unknown",replaceTokens("${}",siteTokens))

  def testFilledUnknown = 
    assertEquals("Unknown",replaceTokens("${filled}",siteTokens))


  def testStartsContentAfter = 
    assertEquals("fred",replaceTokens("${fred",siteTokens))

  def testEmptyUnknownContentAfter = 
    assertEquals("Unknownfred",replaceTokens("${}fred",siteTokens))

  def testFilledUnknownContentAfter = 
    assertEquals("Unknownfred",replaceTokens("${filled}fred",siteTokens))


  def testStartsContentBeforeAndAfter = 
    assertEquals("janefred",replaceTokens("jane${fred",siteTokens))

  def testEmptyUnknownContentBeforeAndAfter = 
    assertEquals("janeUnknownfred",replaceTokens("jane${}fred",siteTokens))

  def testFilledUnknownContentBeforeAndAfter = 
    assertEquals("janeUnknownfred",replaceTokens("jane${filled}fred",siteTokens))



  def testMatchContentBeforeAndAfter = 
    assertEquals("rod, jane and freddy",
		 replaceTokens("rod, ${rainbow} freddy",
			       siteTokens ++ Map("rainbow" -> (()=>"jane and"))))

  def testMatchContentBeforeAndAfterExtra = 
    assertEquals("rod, jane and freddy - with puppets painting",
		 replaceTokens("rod, ${rainbow} freddy ${with} painting",
			       siteTokens ++ Map("rainbow" -> (()=>"jane and"), "with" -> (()=>"- with puppets"))))

  def testSimpleEscape = 
    assertEquals("fred",replaceTokens("fred",siteTokens)) 

  def testEmptyEscape = 
    assertEquals("",replaceTokens("",siteTokens))

  def testStartsEscape = 
    assertEquals("",replaceTokens("${",siteTokens))

  def testEmptyUnknownEscape = 
    assertEquals("Unknown",replaceTokens("${}",siteTokens))

  def testFilledUnknownEscape = 
    assertEquals("Unknown",replaceTokens("${filled}",siteTokens))


  def testStartsContentAfterEscape = 
    assertEquals("fred",replaceTokens("${fred",siteTokens))

  def testEmptyUnknownContentAfterEscape = 
    assertEquals("Unknownfred",replaceTokens("${}fred",siteTokens))

  def testFilledUnknownContentAfterEscape = 
    assertEquals("Unknownfred",replaceTokens("${filled}fred",siteTokens))


  def testStartsContentBeforeAndAfterEscape = 
    assertEquals("janefred",replaceTokens("jane${fred",siteTokens))

  def testEmptyUnknownContentBeforeAndAfterEscape = 
    assertEquals("janeUnknownfred",replaceTokens("jane${}fred",siteTokens))

  def testFilledUnknownContentBeforeAndAfterEscape = 
    assertEquals("janeUnknownfred",replaceTokens("jane${filled}fred",siteTokens))



  def testMatchContentBeforeAndAfterEscape = 
    assertEquals("rod, jane and freddy",
		 replaceTokens("rod, ${rainbow} freddy",
			       siteTokens ++ Map("rainbow" -> (()=>"jane and"))))

  def testMatchContentBeforeAndAfterExtraEscape = 
    assertEquals("rod, ${rainbow} freddy ${with} painting",
		 replaceTokens("rod, \\${rainbow} freddy \\${with} painting",
			       siteTokens ++ Map("rainbow" -> (()=>"jane and"), "with" -> (()=>"- with puppets"))))



  def testOneNesting = 
    assertEquals("rod, jane and freddy", 
		 replaceTokens("${rainbow}", 
			       Map("rainbow" -> "rod, ${others}", "others" -> "jane and freddy")))

  def testRepatedOneNesting = 
    assertEquals("rod, jane and freddy", 
		 replaceTokens("${rainbow}", 
			       Map("rainbow" -> "${rod}, ${others}", "rod" -> "rod", "others" -> "jane and freddy")))

  def testTwoNesting = 
    assertEquals("rod, jane and freddy", 
		 replaceTokens("${rainbow}", 
			       Map("rainbow" -> "${all}", "all"->"${rod}, ${others}", "rod" -> "rod", "others" -> "jane and freddy")))

  def testThreeNesting = 
    assertEquals("rod, jane and freddy", 
		 replaceTokens("${rainbow}", 
			       Map("lastone"->"freddy", "rainbow" -> "${all}", "all"->"${rod}, ${others}", "rod" -> "rod"
				   , "others" -> "jane and ${lastone}" )))

}
