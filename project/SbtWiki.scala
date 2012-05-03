package scales.sbtplugins

import org.eclipse.mylyn.wikitext.core.util.ServiceLocator
import org.eclipse.mylyn.wikitext.core.parser.MarkupParser
import org.eclipse.mylyn.wikitext.core.parser.markup.MarkupLanguage
import org.eclipse.mylyn.wikitext.mediawiki.core.{MediaWikiLanguage, Template}

import sbt._
import IO._
import scales.sbtplugins.Utils._

import java.io.File

import com.tristanhunt.knockoff.DefaultDiscounter._

/**
 * Allows providing an additional header set per individual markup.
 * Title gets special treatment as it is likely the most common case, given the siteHeaders.
 * If the user really only wants a certain set of headers then they can set overrideHeaders to true.
 */ 
case class MarkupHeader( title : String , extraHeaders : String = "", overrideHeader : Boolean = false )(val shortDesc : String = title, val description : String = title)

/**
 * Provides a wrapper around WikiText for simple site operations
 */ 
object SbtWiki {
  
  import Tokens._

  lazy val wikiTextServiceLocator = ServiceLocator.getInstance(this.getClass.getClassLoader)

  sealed trait MarkupType
  case object Markdown extends MarkupType
  case object Textile extends MarkupType
  case object MediaWiki extends MarkupType
  case object Confluence extends MarkupType
  case object TracWiki extends MarkupType
  case object TWiki extends MarkupType

  val markupExtensions = Map("mw" -> MediaWiki, "textile" -> Textile, "mwiki" -> MediaWiki, "trac" -> TracWiki, "md" -> Markdown, "markdown" -> Markdown)

  /**
   * provide a list of templates to a mediawiki langauge.
   *
   * Note its late bound to allow for further cool runtime substituions
   */ 
  val templates = new scala.util.DynamicVariable(List[(String,String)]())

  /**
   * Uses markupExtensions to get the list of file extensions to filter for.
   * All files are copied over, then each extension file is converted to html
   * and the original optionally deleted.
   *
   */
  def copyAndConvert( from : File, to : File, filterOut : Iterable[FileFilter], extraHeaders : String, bodyEnd : String, tokens : Map[String, String], log: Logger, delete : Boolean, documentHeaders : Map[String, MarkupHeader]) : Option[String] = 
    // copy all of them over
    copyDir(from, to, filterOut, log) ~~>
      markupExtensions.foldLeft(None : Option[String]){
	(x,y) => 
	x ~~> {
	  (to ** ("*."+y._1)).get.foldLeft(None : Option[String]){
	    (o, path) => {
	      convert( path, new File(path.getParent  + "/" + path.base + ".html"), {
		val pf = path.getCanonicalPath
		val pb = to.getCanonicalPath
		val fname = pf.substring(pb.length + 1)
		documentHeaders.get(fname).
		      map{ x => (if (x.overrideHeader) ""
			  else extraHeaders)+"<title>"+x.title+"</title>"+x.extraHeaders }.
		      getOrElse{extraHeaders}
	      }
, bodyEnd, tokens, log, delete)
	    }
	  }
	}
      }    

  /**
   * Converts wiki formats to html with a given set of extra headers then removes the original file.  Only use this when the html path is in target :-)
   */ 
  def convert( markup : File, html : File, extraHeaders : String, bodyEnd : String, tokens : Map[String, String], log : Logger) : Option[String] = convert(markup, html, extraHeaders, bodyEnd, tokens, log, true)

  /**
   * Converts wiki formats to html with a given set of extra headers
   */ 
  def convert( markup : File, html : File, extraHeaders : String, bodyEnd : String, tokens : Map[String, String], log : Logger, delete : Boolean) : Option[String] = {
    try {
      import org.eclipse.mylyn.wikitext.core.parser.builder.HtmlDocumentBuilder
      import java.io.StringWriter

      markupExtensions.get(markup.ext).cata( ml => {
	if (ml eq Markdown) {
	  val ms = IO.read(markup, utf8)
	  val msr = toXML(knockoff(replaceTokens(ms,tokens)))
	  val x = "<html><head>"+
	    replaceTokens(extraHeaders,tokens)+
	    "</head><body>"+msr+
	    replaceTokens(bodyEnd,tokens) +
	    "</body></html>"
	  ioCatching{write(html, x, utf8); None}(log)	  
	} else {
	  // mediawiki
        val lang = wikiTextServiceLocator.getMarkupLanguage(ml.toString)

	if (lang.isInstanceOf[MediaWikiLanguage]) {
	  val list = new java.util.ArrayList[Template]()
	  list.addAll(lang.asInstanceOf[MediaWikiLanguage].getTemplates())
	  import scala.collection.JavaConversions._
	  list.addAll(templates.value.map{
	    x=>
	    val (name, markup) = x
	    val template = new Template()
	    template.setName(name)
	    template.setTemplateMarkup(markup)
	    template
	  })
	  lang.asInstanceOf[MediaWikiLanguage].setTemplates(list)
	}

	val writer = new StringWriter
	val builder = new HtmlDocumentBuilder(writer){
	  override def emitHeadContents() {
	    super.emitHeadContents()
	    charactersUnescaped(replaceTokens(extraHeaders,tokens))
	  }

	  override def endBody() {
	    charactersUnescaped(replaceTokens(bodyEnd,tokens))
	    super.endBody()
	  }
	}
	
	val res : Option[String] = ioCatchingRes(read(markup, utf8))(log).fold(Some(_), { mu => 
	  val replaced = replaceTokens(mu, tokens)
	  ioCatching{write(html.asFile, {
	    val parser = new MarkupParser(lang)// Thread Safety/reentrance of the language?
	    parser.setBuilder(builder)
	    parser.parse(replaced)
	    writer.toString()
	  }, utf8); None}(log)
        })
	res
	}
      }, Some("Could not find markup language for extension "+markup.ext) )
    } catch {
      case e : Exception => e.printStackTrace;Some("Could not convert markup: " + e.getMessage)
    }
  } ~~> {if (delete) 
	   if (markup.asFile.delete) 
	     None
	   else Some("Could not delete "+markup)
	 else None}

}
