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
  val siteInfoProject = SettingKey[LocalProject]("site-info-project")

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

  /**
   * A list of markup docs that will be shown in the index.
   * The pair is relative filename without extension -> description.
   */ 
  val siteMarkupDocs = SettingKey[ List[(String, String)] ]("site-markup-docs")

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
   * Defaults to a markup file called menubar.mw in siteResourceDir
   */ 
  val menuBar = SettingKey[ File ]("site-menu-bar")

  /**
   * Override to provide different js behaviour for the menubar
   */ 
  val menuBarJS = SettingKey[ String ]("site-menu-bar-js")

  /**
   * Default output for the site (site:target)??
   */ 
  val siteOutputPath = SettingKey[File]("site-output-path")

  private[sbtplugins] case class SiteParams(siteOutputPath : File, siteIgnore : Seq[FileFilter], sites : Seq[Site], siteIndexHeader : File, siteIndexFooter : File, siteMarkupDocs : List[(String, String)], siteCSS : File, packageSiteZip : File, outputPath : File, siteTokens : Map[String, ()=>String] = Map(), siteHeaders : String = "")

  private[sbtplugins] val siteParams = SettingKey[SiteParams]("obfuscated.site-params")

  private[sbtplugins] case class SiteDocParams(siteResourceDir : File, siteJQuery : File, resourcesOutDir : File, siteDocsIgnore : Seq[FileFilter], siteMarkupDelete : Boolean = false, siteMarkupDocHeaders : Map[String, MarkupHeader], siteMediaWikiTemplates : List[(String, String)])

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
 */
case class Site( copy : Copy, siteDesc : String, siteLink : Option[String], ignore : Iterable[FileFilter])

object Site{
  def apply( copy : Copy, siteDesc : String ) : Site = Site(copy,siteDesc, None, List())

  def apply( copy : Copy, siteDesc : String, siteLink : String ) : Site = Site(copy,siteDesc, Some(siteLink), List())

  def apply( copy : Copy, siteDesc : String, siteLink : Option[String] ) : Site = Site(copy,siteDesc, siteLink, List())

  def apply( copy : Copy, siteDesc : String, siteLink : String, ignore : Iterable[FileFilter] ) : Site = Site(copy,siteDesc, Some(siteLink), ignore)

}


object DefaultSites {

  val xraySite = Site(Copy("api.sxr"), "Scala XRay Specs view of the source, also available via the Scaladocs")
  val docsSite = Site(Copy("api", "doc"), "ScalaDocs for ${artifactID}")
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

  def settings(infoProject : LocalProject) = Seq(
    siteInfoProject := infoProject,
    siteCSS <<= baseCss,
    siteJQuery <<= (resourcesOutDir) apply { _ / "jquery-1.5.min.js"},
    sites := List(xraySite, docsSite),
    siteResourceDir <<= sourceDirectory in compile,
    siteIndexHeader <<= siteResourceDir apply { _ / "siteIndexHeader.mw" },
    siteIndexFooter <<= siteResourceDir apply { _ / "siteIndexFooter.mw" },
    menuBar <<= siteResourceDir apply { _ / "menubar.mw" },
    menuBarJS := menuBarJSDefault,
    siteTokens <<= (version in siteInfoProject, moduleName in siteInfoProject, organization in siteInfoProject) apply getSiteTokens,
    siteMarkupDelete := true,
    siteMarkupDocHeaders <<= (menuBar, menuBarJS) apply { (f,j) => Map(f.name -> MarkupHeader("Menu Bar", css("menubar.css") + j ))},
    highlightScripts := getHighlightScripts,
    siteHeaders <<= (highlightStyle, highlightScripts) apply {
      (s, sc) => 
      css("./scales_base.css") + css("./site_docs.css") + s + js("./jquery.js") + sc
    },
    siteMediaWikiTemplates := List(("code","""<code class="{{{lang}}}"><pre>{{{2}}}</pre></code>""")),
    highlightStyle := css("./highlight/styles/idea.css"),
    siteIgnore <<= (siteIndexHeader, siteIndexFooter) apply { (h,f) => List(new ExactFilter(h.name), new ExactFilter(f.name), new ExactFilter(".svn"), // all svn files
GlobFilter("*.*~")) }, // all emacs backups
    siteDocsIgnore <<= siteIgnore,
    siteOutputPath <<= (crossTarget in compile) apply { _ / "site" },
    packageSiteZip := new java.io.File("C:/root.zip"),
    siteParams <<= (siteOutputPath, siteIgnore, sites, siteIndexHeader, siteIndexFooter, siteMarkupDocs, siteCSS, packageSiteZip, crossTarget in compile) apply {
      SiteParams(_,_,_,_,_,_,_,_,_)
    },
    siteParams <<= (siteParams, siteTokens, siteHeaders) apply { (a, b, c) => a.copy(siteTokens = b, siteHeaders = c) },
    siteDocParams <<= (siteResourceDir, siteJQuery, resourcesOutDir, siteDocsIgnore, siteMarkupDelete, siteMarkupDocHeaders, siteMediaWikiTemplates) apply SiteDocParams,
    siteDocs <<= (siteDocParams, siteParams, unpackResourcesTask, streams) map makeSite,
    site <<= (siteParams, streams, siteDocs, unpackResourcesTask) map siteTask
  )

  def getSiteTokens(version : String, name: String, org : String) = Map[String, ()=>String]( "User" -> userF, "timestamp" -> { () => {new java.util.Date().toString}}, 
		   "datetime" -> {() => {java.text.DateFormat.getDateTimeInstance.format(new java.util.Date())}},
//		   "moduleID" -> {() => id},
		   "projectName" -> {() => name},
		   "projectVersion" -> {() => version},
		   "projectOrganization" -> {() => org}, 
		   "projectOrganisation" -> {() => org}, 
		   "FullVersion" -> {() => org + " : " + name + "-" + version},
		   "cscala" -> cbs("scala"), 
		   "bxml" -> (() => "<code class=\"xml\">"),
		   "cxml" -> cbs("xml"),
		   "exml" -> (() => "</code>"),
		   "cjava" -> cbs("java"),
		   "cend" -> cend
		     ) ++ sysEnv ++ sysProperties

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
 
  def makeSite(params : SiteDocParams, siteParams : SiteParams, unpackResources : Option[String], streams : std.TaskStreams[_]) =  { // TODO - allow the source zip for highlighting and site_docs to be replaceds
    import params._
    import siteParams._
    val log = streams.log

    ( if (siteResourceDir.exists) None else Some("The site directory "+siteResourceDir+" does not exist, please create it, or override siteResourceDir before calling site.") ) ~~>
    unpackResources ~~>
    Utils.copyFile(siteCSS, siteOutputPath / "scales_base.css", log) ~~>
    Utils.copyFile(siteJQuery, siteOutputPath / "jquery.js", log) ~~>
    Utils.copyFile(resourcesOutDir./("site_docs.css"), siteOutputPath / "site_docs.css", log) ~~>
    {ioCatching{unzip(resourcesOutDir./("highlight.zip"), siteOutputPath);None}(log) } ~~> 
    SbtWiki.templates.withValue(siteMediaWikiTemplates) {
      copyAndConvert(siteResourceDir, siteOutputPath, siteDocsIgnore, 
	  siteHeaders, siteTokens, log, siteMarkupDelete, siteMarkupDocHeaders)
    }
  }

  def siteTask( siteParams : SiteParams, streams : std.TaskStreams[_], siteDocRes : Option[String], unpackResources : Option[String] ) = siteDocRes.map{ s => streams.log.error("Cannot make site as siteDocs has failed due to "+s); s} ~~> {
    // create target dir
    import siteParams._
    import streams.log
    import Path._

    val outDir = siteOutputPath
    val siteIndex = outDir / "index.html"
    val siteIndexMd = outDir / "index.mw"
    val siteIndexMdf = siteIndexMd.asFile

    val siteIgnores = siteIgnore

    // doesn't matter if it fails
    IO.createDirectory(outDir)

    val sitesc = sites
    // copy them over
    sitesc.foldLeft( None : Option[String]){
      (x, y) =>
	x ~~> {
	  val from = new java.io.File(outputPath.asFile, y.copy.from)
	  val to = new java.io.File( outDir.asFile, y.copy.to)
	  if (from.exists)
	    copyDir(from, to, siteIgnores ++ y.ignore, log) 
	  else {
	    log.debug("Could not find directory "+from.getAbsolutePath+" skipping copy")
	    None
	  }
	}
    } ~~> {
      // create cover page..
      val wikiBase = if (siteIndexHeader.exists) ioCatchingRes(read(siteIndexHeader.asFile, utf8))(log)
	else Right("= ${FullVersion} Site =\n")
      wikiBase.fold(Some(_),{b => ioCatching{write(siteIndexMdf, b+"\n<br/>\n", utf8);None}(log) ~~> {
	// middle
	sitesc.foldLeft( None : Option[String] ){
	  (x,y) =>
	    x ~~> {
	      if (new java.io.File(outputPath.asFile, y.copy.from).exists) {
		val link = y.siteLink.getOrElse{"./"+y.copy.to+"/index.html"}
		append(siteIndexMdf, "* ["+link+" "+y.siteDesc+"]\n", utf8, log)
	      } else x
	    }
	} ~~> (
	  if (siteMarkupDocs.size > 0) {
	    append(siteIndexMdf, "\n<br/>\n== Project Documentation ==\n<br/>\n", utf8, log)
	    siteMarkupDocs.foldLeft( None : Option[String] ){
	     (x,y) =>
	     x ~~> {
	       val f = new java.io.File(outDir.asFile, y._1 + ".html") 
	       if (f.exists) {
		 append(siteIndexMdf, "* [./"+y._1+".html "+y._2+"]\n", utf8, log)
	       } else Some("File "+f.getCanonicalPath + " does not exist")
	     }
	    }
	  } else None
	) ~~> {
	  // footer
	  val wikiEnd = if (siteIndexFooter.exists) ioCatchingRes(read(siteIndexFooter.asFile, utf8))(log)
			else Right("<br/>\n[./"+packageSiteZip.name+" Download Site Zip]\n\nBuilt By: ${User}, ${timestamp}")
	  wikiEnd.fold(Some(_),{f=>append(siteIndexMdf, f, utf8, log)})
	}
      }}) ~~> {
	// convert to html
	try{
	  unpackResources ~~>
	  copyFile(siteCSS, outDir / "scales_base.css", log) ~~>
	  convert(siteIndexMd, siteIndex, title("${FullVersion} Site") + siteHeaders, siteTokens, log)
	} catch {
	  case e : Exception => Some(e.getMessage)
	}
      }
    }
  }

  val menuBarJSDefault = js("./jquery.js")+"""
  <script type="text/javascript">
  $(function(){
  $('li:has(ul)').toggleClass('menuPlus');
  $('ul').children('li:has(ul)').click(function(event){
            if (this == event.target) {                
                $(this).children('ul').toggle('fast');
		$(this).toggleClass('menuPlus');
		$(this).toggleClass('menuMinus');		
            }
            return false;
        })
        .children('ul').hide();
});
</script>
"""

}
