package scales.sbtplugins

import sbt._
import IO.{unzip, read, utf8, write}
import Utils._
import SbtWiki._

object SiteKeys {

  /**
   * location of the zip file
   */
  val packageSiteZip = SettingKey[File]("site-package-zip")

  /**
   * The project used to fill the tokens, e.g. version number org etc
   */
  val siteInfoProject = SettingKey[Project]("site-info-project")

  /**
   * Site wide base css
   */ 
  val siteCSS = SettingKey[File]("site-css")

  /**
   * Site wide base jquery
   */
  val siteJQuery = SettingKey[File]("site-jquery")

  /**
   * Which sites should be published
   */ 
  val sites = SettingKey[Seq[Site]]("site-sites")

  val siteResourceDir = SettingKey[File]("site-resource-dir")

  val siteIndexHeader = SettingKey[File]("site-index-header")
  val siteIndexFooter = SettingKey[File]("site-index-footer")

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
    import scala.collection.JavaConversions._
    System.getenv().map{(x) => (x._1, () => x._2)}
  }

  val sysProperties = {
    import scala.collection.JavaConversions._
    System.getProperties().map{(x) => (x._1.toString, () => x._2.toString)}
  }
  
  val siteTokens = SettingKey[Map[String, ()=>String]]("site-tokens")

  /**
   * By default deletes markup files after calling site-docs
   */ 
  val siteMarkupDelete = SettingKey[Boolean]("site-markup-delete")

  /**
   * Override to change/disable the use of highlight.
   *
   * NOTE: It does some crazy looking stuff in here, but its
   * due to bugs in <IE9 that strip newlines out of pre's.
   * The outerHTML trick gets around that limitation.
   *
   * In both cases we wrap a code in the middle, letting highlight.js do its
   * thing.
   */
  val highlightScripts = SettingKey[String]("site-highlight-scripts")
 
  /**
   * String representing the headers for each generated site html.
   *
   * Defaults to the site wide CSS, includes a jquery (override siteJQuery to change versions etc) and adds highlight.js (override sitHightlightStyle to change the style used).  NOTE as a default only xml, java and scala are provided
   */ 
  val siteHeaders = SettingKey[String]("site-headers")

  val siteMediaWikiTemplates = SettingKey[List[(String, String)]]("site-mediawiki-templates")
  
  /**
   * Override to change the highlight style used during the site-docs run.
   */
  val highlightStyle = SettingKey[String]("site-highlight-style")

  /**
   * List of files to ignore when generating the site docs, defaults to siteIgnore
   */ 
  val siteDocsIgnore = SettingKey[Seq[FileFilter]]("site-docs-ignore")
  
  /**
   * Generates user documentation from the
   * src/site directory (configure with siteDocsBase).
   * Whatever is in there is copied to target and all known markup
   * extensions are converted.
   *
   * Note by default it will delete all original markup
   * files after conversion.
   * Use siteMarkupDelete to control this behaviour.
   *
   * @returns None when it runs successfully and Some(String) when not
   */
  val siteDocs = TaskKey[Option[String]]("site-docs")

  case class SiteIndex( menuBar : String, indexPage : String)

  /**
   * An html index of the generated files and main doc links, also copys the generated files over
   */ 
  val siteIndex = TaskKey[SiteIndex]("site-index")

  /**
   * A list of markup docs, without extension, that will be shown in the index.
   * The path is relative to src.
   */ 
  val siteMarkupDocs = SettingKey[ Seq[String] ]("site-markup-docs")

  /**
   * Users wishing to add a title or override other headers should
   * add their markup document here, relative filename to MarkupHeader
   */
  val siteMarkupDocHeaders = SettingKey[ Map[String, MarkupHeader] ]("site-markup-doc-headers")

  /**
   * List of files to ignore when building a site
   */ 
  val siteIgnore = SettingKey[ Seq[FileFilter] ]("site-ignore")

  /**
   * Produces the site to siteOutputPath, returning None when successful, depends on site-docs
   */ 
  val site = TaskKey[ Option[String] ]("site-site")

  /**
   * Defaults to a markup file called menubar.mw in siteResourceDir.
   * When this file exists the menu bar will be added to all Site generated files.
   */ 
  val menuBar = SettingKey[ File ]("site-menu-bar")

  /**
   * Generates the html for the menu bar, returning left for menu bar and right for an error.
   */ 
  val menuBarHtml = TaskKey[ Either[String, String] ]("site-menu-bar-html")

  /**
   * Override to provide different js behaviour for the menubar
   */ 
  val menuBarJS = SettingKey[ String ]("site-menu-bar-js")

  /**
   * Override to provide a different css File for the menubar
   */ 
  val menuBarCSS = SettingKey[ File ]("site-menu-bar-css")

  /**
   * This is added to the bottom of all generated pages.
   * By default it will include the content of the menuBar
   */ 
  val siteBodyEnd = TaskKey[ String ]("site-body-end")

  /**
   * Default output for the site (site:target)??
   */ 
  val siteOutputPath = SettingKey[File]("site-output-path")

  private[sbtplugins] case class SiteParams(siteOutputPath : File, siteIgnore : Seq[FileFilter], sites : Seq[Site], siteIndexHeader : File, siteIndexFooter : File, siteCSS : File, siteResourceDir : File, packageSiteZip : File, outputPath : File, siteTokens : Map[String, ()=>String] = Map(), siteHeaders : String = "", siteMarkupDocHeaders : Map[String, MarkupHeader] = Map(), siteMarkupDocs : Seq[String] = List())

  private[sbtplugins] val siteParams = SettingKey[SiteParams]("obfuscated.site-params")

  private[sbtplugins] case class SiteDocParams(siteJQuery : File, resourcesOutDir : File, siteDocsIgnore : Seq[FileFilter], siteMarkupDelete : Boolean = false, siteMediaWikiTemplates : List[(String, String)])

  private[sbtplugins] val siteDocParams = SettingKey[SiteDocParams]("obfuscated.site-doc-params")
}
/*
trait ArtifactsHelper extends BasicManagedProject {
  self : SiteProject =>

  val siteArtifact = Artifact(artifactID, "zip", "zip", Some("site"), Nil, None)
  def packageSiteZip = defaultJarPath("-site.zip")
  
  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("docs"), 
Nil, None)
  
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), 
Nil, None)
  
  val publishScalaDocs = true
  val publishSiteZip = true
  val publishSources = true

  override def artifacts =
   super.artifacts ++ {
     if (publishSiteZip) Seq( siteArtifact )
     else Seq()
   } ++ {
     if (publishScalaDocs) Seq( docsArtifact )
     else Seq()
   } ++ {
     if (publishSources) Seq( sourceArtifact )
     else Seq()
   }
  
  override def packageToPublishActions = Seq(packageSite, packageDocs, packageSrc) ++ super.packageToPublishActions
}

*/
case class Copy( from : String, to : String )
object Copy {
  def apply( from : String ) : Copy = Copy(from, from)
}

/**
 * siteDesc is a mediawiki snippet (will simply be embedded)
 * to represent the part of the site that copy refers to.
 * siteLink is optionally the target html to load for that link,
 * defaulting to copy.to / index.html.
 *
 * It MUST be in utf-8 format
 *
 * The ignore list can be tailored for just a site, it will be added to 
 * the siteIgnore list upon running site.
 *
 * shortDesc is used for the menu bar, siteDesc for the main index  
 */
case class Site( copy : Copy, shortDesc : String, siteDesc : String, siteLink : Option[String] = None, ignore : Iterable[FileFilter] = List())

object DefaultSites {

  val xraySite = Site(Copy("api.sxr"), "SXR Source", "Scala XRay Specs view of the source, also available via the Scaladocs")
  val docsSite = Site(Copy("api", "doc"), "ScalaDocs", "ScalaDocs for ${artifactID}")
/*  val xrayTestsSite = Site(Copy("test-classes.sxr"), "Scala XRay Specs view of the test source")
  val scctSite = Site(Copy("coverage-report"), "SCCT - Scala TestCoverage")
  // here we don't have an index just the dir
  val surefireRepSite = Site(Copy("surefire-reports"), "Surefire Reports", "surefire-reports/")
  val testReviewSite = Site(Copy("test-review"), "Test Review Results")
*/

}

/**
 * Utilities and tasks for creating a site
 */ 
object SiteSettings {

  import SiteKeys._
  import DefaultSites._
  import Keys._

  val cend = () => "</pre>"

  /**
   * Call to add code@lang pre for cXX tokens 
   */
  def cbs(lang : String) = () => "<pre class=\"language-"+lang+"\">"

  def settings(infoProject : Project) = Seq(
    siteInfoProject := infoProject,
    siteCSS <<= baseCss,
    siteJQuery <<= (resourcesOutDir) apply { _ / "jquery-1.5.min.js"},
    sites := List(xraySite, docsSite),
    siteResourceDir <<= sourceDirectory in compile,
    siteIndexHeader <<= siteResourceDir apply { _ / "siteIndexHeader.mw" },
    siteIndexFooter <<= siteResourceDir apply { _ / "siteIndexFooter.mw" },
    menuBar <<= siteResourceDir apply { _ / "menubar.mw" },
    menuBarCSS <<= siteResourceDir apply { _ / "menubar.css" },
    menuBarJS := menuBarJSDefault,
    //siteTokens <<= (version in siteInfoProject, moduleName in siteInfoProject, organization in siteInfoProject) apply getSiteTokens,
    siteTokens <<= (projectID in infoProject) apply getSiteTokens,
    siteMarkupDelete := true,
    siteMarkupDocs := List(),
    siteMarkupDocHeaders := Map(),
    highlightScripts := getHighlightScripts,
    siteHeaders <<= (highlightStyle, highlightScripts, menuBarJS ) apply {
      (s, sc, ms) => "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=8\" />" +
      css("./scales_base.css") + css("./site_docs.css") + s + js("./jquery.js") + sc + css("./menubar.css") + ms
    },
    siteMediaWikiTemplates := List(("code","""<code class="{{{lang}}}"><pre>{{{2}}}</pre></code>""")),
    highlightStyle := css("./highlight/styles/idea.css"),
    siteIgnore <<= (siteIndexHeader, siteIndexFooter) apply { (h,f) => List(new ExactFilter(h.name), new ExactFilter(f.name), new ExactFilter(".svn"), // all svn files
GlobFilter("*.*~")) }, // all emacs backups
    siteDocsIgnore <<= siteIgnore,
    siteOutputPath <<= (crossTarget in compile) apply { _ / "site" },
    packageSiteZip := new java.io.File("C:/root.zip"),
    siteParams <<= (siteOutputPath, siteIgnore, sites, siteIndexHeader, siteIndexFooter, siteCSS, siteResourceDir, packageSiteZip, crossTarget in compile) apply {
      SiteParams(_,_,_,_,_,_,_,_,_)
    },
    siteBodyEnd := "",
    siteParams <<= (siteParams, siteTokens, siteHeaders, siteMarkupDocHeaders, siteMarkupDocs) apply { (a, b, c, d, e) => a.copy(siteTokens = b, siteHeaders = c, siteMarkupDocHeaders = d, siteMarkupDocs = e) },
    siteDocParams <<= (siteJQuery, resourcesOutDir, siteDocsIgnore, siteMarkupDelete, siteMediaWikiTemplates) apply SiteDocParams,
    siteIndex <<= (siteParams, streams) map getSiteIndex,
    menuBarHtml <<= (siteParams, menuBar, siteIndex, streams) map getMenuBar,
    siteDocs <<= (siteDocParams, siteParams, siteBodyEnd, menuBarHtml, unpackResourcesTask, streams) map makeSite,
    site <<= (siteParams, streams, siteDocs, siteBodyEnd, menuBarHtml, unpackResourcesTask, siteIndex) map siteTask
  )

//  def getSiteTokens(version : String, name: String, org : String) = 
  def getSiteTokens(module : ModuleID) = {
    import module._

Map[String, ()=>String]( "User" -> userF, "timestamp" -> { () => {new java.util.Date().toString}}, 
		   "datetime" -> {() => {java.text.DateFormat.getDateTimeInstance.format(new java.util.Date())}},
		   "artifactID" -> {() => name + "-" + revision},
		   "moduleID" -> {() => name + "-" + revision},
		   "projectName" -> {() => name},
		   "projectVersion" -> {() => revision},
		   "projectOrganization" -> {() => organization}, 
		   "projectOrganisation" -> {() => organization}, 
		   "FullVersion" -> {() => organization + " : " + name + "-" + revision},
		   "cscala" -> cbs("scala"), 
		   "bxml" -> (() => "<code class=\"xml\">"),
		   "cxml" -> cbs("xml"),
		   "exml" -> (() => "</code>"),
		   "cjava" -> cbs("java"),
		   "cend" -> cend
		     ) ++ sysEnv ++ sysProperties
  }

  def getHighlightScripts =  
      js("./highlight/highlight.pack.js") +
      """<script type="text/javascript">
$(function() {
$("pre[class^='language-']").each(function(i,elem) {
  var clazz = $(elem).attr('class');
  var str = elem.innerHTML//.replace(/\r\n|\r|\n/g,"<br/>");
  // Workaround for IE <PRE> innerHTML normalization quirk
  if (elem.tagName == "PRE" && "outerHTML" in elem)
    {
      elem.outerHTML = "<PRE><CODE class='"+clazz+"'>" + str + "</CODE></PRE>";
    }
  else
    {
      elem.innerHTML = "<CODE class='"+clazz+"'>" + str + "</CODE>"; //.replace(/\r\n|\r|\n/g,"<br/>");;
    }
});
  hljs.initHighlighting();
});
</script>
"""

/*  lazy val packageSite = packageSiteAction
  def packageSiteAction = zipTask(siteOutputPath ** "*", packageSiteZip).dependsOn(site)

  lazy val sitePublishRelative = getSitePublishRelative 
  // by default we don't want the release to show any intermediate builds for the docs
  def getSitePublishRelative = projectOrganization.value +"/"+ artifactID +"/"+ (projectVersion.value.toString.replaceAll("-.*",""))

  // paths don't like relatives and forward slashes
  lazy val siteDeployFile = getSiteDeployFile
  def getSiteDeployFile = new java.io.File("./../sites").getAbsoluteFile()

  /**
   * List of files to ignore when deploying the site, defaults to siteIgnore
   */ 
  def deploySiteIgnore = siteIgnore

  /**
   * Deploys the zip file with a locally useable site and the remote with further changed paths
   */ 
  lazy val deploySite = deploySiteAction
  def deploySiteAction = task {
    // make dirs, copy over the site then zip
    import FileUtilities._
    import java.io.File
    val dir = new File(siteDeployFile, sitePublishRelative).getCanonicalFile
    val parent = dir.getParentFile
    val sitesrc = siteOutputPath.asFile
    val sitezip = packageSiteZip.asFile.getCanonicalFile
    println("Attempting to publish from "+sitesrc.getCanonicalPath+" with zip "+sitezip.getCanonicalPath)
    
    copyDir(sitesrc, dir, deploySiteIgnore, log) ~~>
      copyFile(sitezip, new File(dir, sitezip.getName), log)
  } dependsOn(packageSite)

  /**
   * Override to change the tasks executed before the site is run.
   */ 
  def siteAllActions = "clean;test-coverage;test-review;doc"
  
  def canFailActions = List("test-coverage","test-review")

  def ignoreFailActionsForSite = true

  lazy val siteAll = siteAllAction
  def siteAllAction = task {
    callCommands(siteAllActions+";site", this, if (ignoreFailActionsForSite) canFailActions else Nil)
  }
*/

  def getMenuBar( siteParams: SiteParams, menuBarFile : File, siteIndex : SiteIndex, stream : std.TaskStreams[_]) : Either[String, String] = {
    import siteParams._
    import stream.log

    if (menuBarFile.exists) {
      // generate the menu bar 
      val th = java.io.File.createTempFile("scales_sit_mb_","_mb")
      try{
	val r = convert(menuBarFile, th, siteHeaders, "", siteTokens, log, 
		false // not copying
		)
	r.map{ x => log.error("Menubar file could not be generated "+x); Right(x) }.getOrElse{
	  // transformation worked, grab the body...
	  val full = IO.read(th, utf8)
	  val p = full.indexOf("<body>") // NB not sure if this changes per type
	  val ep = full.lastIndexOf("</body>")
	  if ((p < 0) || (ep < 0)) {
	    val r = "Could not find a body in the menu bar ("+p+","+ep+")"
	    log.error(r)
	    Right(r)
	  } else {
	    val innerM = full.substring(p+ 6, ep)
	    Left("<div id=\"scales-site-menubar\">"+siteIndex.menuBar+innerM+"</div>")
	  }
	}
      } finally {
	th.delete
      }
    } else {
      Left("") // doesn't exist so don't add any extra to the output
    }
  }

  def getSiteIndex( siteParams: SiteParams, stream : std.TaskStreams[_]) : SiteIndex = {
    def inner(siteDesc : Site => String, docDesc : MarkupHeader => String) : (String, String) = {
    import siteParams._
    import stream.log

    val sb = new java.lang.StringBuilder()

    sb.append("== Generated Documentation ==\n")

    val sitesc = sites
    // copy them over
    sitesc.foldLeft( None : Option[String]){
      (x, y) =>
	x ~~> {
	  val from = new java.io.File(outputPath.asFile, y.copy.from)
	  val to = new java.io.File( siteOutputPath.asFile, y.copy.to)
	  if (from.exists)
	    copyDir(from, to, siteIgnore ++ y.ignore, log)
	  else {
	    log.debug("Could not find directory "+from.getAbsolutePath+" skipping copy")
	    None
	  }
	}
    }.map(x=>(x,"")) ~~> {
      sitesc.foldLeft( sb ){
	(x,y) =>
	  if (new java.io.File(outputPath.asFile, y.copy.from).exists) {
	    val link = y.siteLink.getOrElse{"./"+y.copy.to+"/index.html"}
	    x.append("* ["+link+" "+ siteDesc(y) +"]\n")
	  } else x
      }
      if (siteMarkupDocs.size > 0) {
	sb.append( "\n== Documentation Highlights ==\n")
	siteMarkupDocs.foldLeft( sb ){
	  (x,y) =>
	    siteMarkupDocHeaders.get(y).map{ 
	      header =>
		
		val f = new java.io.File(siteResourceDir, y)
		if (f.exists) {
		  x.append("* [./"+f.base+".html "+docDesc(header)+"]\n")
		} else x
	    }.getOrElse(x)
	}
      }
      
      None
    }.orElse{
      
      val mu = sb.toString

      // generate the index 
      val th = java.io.File.createTempFile("scales_site_index_","_si")
      val fh = java.io.File.createTempFile("scales_site_index_from",".mw")
      try{
	IO.write(fh, mu, utf8)

	val r = convert(fh, th, siteHeaders, "", siteTokens, log, 
			false // not copying
		      )
	Some((r.map{ x => log.error("Generated header could not be generated "+x); "" }.getOrElse{
	  // transformation worked, grab the body...
	  val full = IO.read(th, utf8)
	  val p = full.indexOf("<body>") // NB not sure if this changes per type
	  val ep = full.lastIndexOf("</body>")
	  if ((p < 0) || (ep < 0)) {
	    val r = "Could not find a body in the site index ("+p+","+ep+")"
	    log.error(r)
	    ""
	  } else {
	    full.substring(p+ 6, ep)
	  }
	}, mu))
      } finally {
	fh.delete
	th.delete
      }
    }
  }.getOrElse(("",""))
   
  SiteIndex(inner(_.shortDesc, _.shortDesc)._1,inner(_.siteDesc, _.description)._2)}

 
  def makeSite(params : SiteDocParams, siteParams : SiteParams, siteBodyEnd : String, menuBarHtml : Either[String, String], unpackResources : Option[String], streams : std.TaskStreams[_]) =  { // TODO - allow the source zip for highlighting and site_docs to be replaceds
    import params._
    import siteParams._
    import streams.log
    val bodyEnd = siteBodyEnd + menuBarHtml.fold( x=>x, _ => "");

    ( if (siteResourceDir.exists) None else Some("The site directory "+siteResourceDir+" does not exist, please create it, or override siteResourceDir before calling site.") ) ~~>
    unpackResources ~~>
    Utils.copyFile(siteCSS, siteOutputPath / "scales_base.css", log) ~~>
    Utils.copyFile(siteJQuery, siteOutputPath / "jquery.js", log) ~~>
    Utils.copyFile(resourcesOutDir./("site_docs.css"), siteOutputPath / "site_docs.css", log) ~~>
    {ioCatching{unzip(resourcesOutDir./("highlight.zip"), siteOutputPath);None}(log) } ~~> 
    SbtWiki.templates.withValue(siteMediaWikiTemplates) {
      copyAndConvert(siteResourceDir, siteOutputPath, siteDocsIgnore, 
	  siteHeaders, bodyEnd, siteTokens, log, siteMarkupDelete, siteMarkupDocHeaders)
    }
  }

  def siteTask( siteParams : SiteParams, streams : std.TaskStreams[_], siteDocRes : Option[String], siteBodyEnd : String, menuBarHtml : Either[String, String], unpackResources : Option[String], siteIndexO : SiteIndex ) = siteDocRes.map{ s => streams.log.error("Cannot make site as siteDocs has failed due to "+s); s} ~~> {
    // create target dir
    import siteParams._
    import streams.log
    import Path._

    val bodyEnd = siteBodyEnd + menuBarHtml.fold( x=>x, _ => "");

    val outDir = siteOutputPath
    val siteIndex = outDir / "index.html"
    val siteIndexMd = outDir / "index.mw"
    val siteIndexMdf = siteIndexMd.asFile

    val siteIgnores = siteIgnore

    // doesn't matter if it fails
    IO.createDirectory(outDir)

    // create cover page..
    val wikiBase = if (siteIndexHeader.exists) ioCatchingRes(read(siteIndexHeader.asFile, utf8))(log)
	else Right("= ${FullVersion} Site =\n")
    wikiBase.fold(Some(_),{b => ioCatching{write(siteIndexMdf, b, utf8);None}(log) ~~> {
      // middle
      append(siteIndexMdf, siteIndexO.indexPage, utf8, log)
    } ~~> {
	// footer
	val wikiEnd = if (siteIndexFooter.exists) ioCatchingRes(read(siteIndexFooter.asFile, utf8))(log)
		      else Right("<br/>\n[./"+packageSiteZip.name+" Download Site Zip]\n\nBuilt By: ${User}, ${timestamp}")
	wikiEnd.fold(Some(_),{f=>append(siteIndexMdf, f, utf8, log)})
      }
    }) ~~> {
      // convert to html
      try{
	unpackResources ~~>
	copyFile(siteCSS, outDir / "scales_base.css", log) ~~>
	convert(siteIndexMd, siteIndex, title("${FullVersion} Site") + siteHeaders, bodyEnd, siteTokens, log)
      } catch {
	case e : Exception => Some(e.getMessage)
      }
    }
  }


  val menuBarJSDefault = """
  <script type="text/javascript">
  $(function(){

  $('#scales-site-menubar li:has(ul)').toggleClass('menuPlus');
  $('#scales-site-menubar ul').children('li:has(ul)').click(function(event){
            if (this == event.target) {                
                $(this).children('ul').fadeToggle('fast');
		$(this).toggleClass('menuPlus');
		$(this).toggleClass('menuMinus');
            }
	    if ($(event).get(0).target.tagName.toLowerCase() == "a") {
	      return true; // we want links to be clickable
	    } else {
              return false;
	    }
        })
        .children('ul').hide();

});
</script>
"""

}
