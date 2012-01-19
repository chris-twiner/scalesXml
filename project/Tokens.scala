package scales.sbtplugins


object Tokens {
  
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
   * allowing for all kinds of crazy meta-property action.  Main purpose is of course
   * config files with default templates.
   *
   * Note if you really want an ${ to appear after expansion you had best \ escape it enough, each inner
   * replaceTokens call will scrap one of the \ escapes.
   */ 
  def expand( tokens : Map[String, String] ) : Map[String, String] = {
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

}
