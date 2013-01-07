import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import Defaults._

import scales.sbtplugins._
import SiteKeys.{siteCSS, siteResourceDir, 
		 siteMarkupDocs,
		 siteMarkupDocHeaders, 
		 menuBarTitle}

//import de.johoop.jacoco4sbt._
//import JacocoPlugin._

object ScalesXmlRoot extends Build {

  object sonatype extends org.improving.PublishToSonatype(ScalesXmlRoot) {
    def projectUrl    = "https://www.github.com/chris-twiner/scalesXml"
    def developerId   = "chris.twiner"
    def developerName = "Chris Twiner" // TODO - take from the userprops
    override def licenseUrl    = "http://www.opensource.org/licenses/Apache-2.0"
    override def licenseName   = "Apache-2.0"
    // Override more to taste
  }

  lazy val root = Project("scales-xml-root", file("."), settings = standardSettings ++ dontPublishSettings) aggregate(core, coreTests, jaxen, saxonTests, jaxenTests
, aalto, aaltoTests)

  lazy val core = Project("scales-xml", file("core"), settings = standardSettings ++ deployOrg)

  lazy val coreTests = Project("scales-xml-tests", file("core-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(core)

  lazy val saxonTests = Project("saxon-tests", file("saxon-tests"), settings = standardSettings ++ dontPublishSettings ++ dontBuildIn28) dependsOn(coreTests % "test->test")

  lazy val jaxen = Project("scales-jaxen", file("jaxen"), settings = standardSettings ++ deployOrg) dependsOn(core)

  lazy val jaxenTests = Project("jaxen-tests", file("jaxen-tests"), settings = standardSettings ++ dontPublishSettings ++ dontBuildIn28) dependsOn(jaxen % "compile->test", coreTests % "test->test") //  % "compile->compile;test->test"

  lazy val aalto = Project("scales-aalto", file("aalto"), settings = standardSettings ++ deployOrg) dependsOn(core)

  lazy val aaltoTests = Project("aalto-tests", file("aalto-tests"), settings = standardSettings ++ dontPublishSettings ++ dontBuildIn28) dependsOn(aalto % "compile->test", coreTests % "test->test")

  /* project that sucks in the others like fullDocsAndSxr for the purpose of coverage tests
  lazy val coverageProject = {
    val sProjs = Seq(core, jaxen)
    val tProjs = Seq(coreTests, jaxenTests)
//    val spaths = (sources in Compile) <<= (sProjs.map(sources in Compile in _).join).map(_.flatten)
//    val tpaths = (sources in Test) <<= (tProjs.map(sources in Test in _).join).map(_.flatten)

//    val cp = (externalDependencyClasspath in Compile) <<= (sProjs.map(externalDependencyClasspath in Compile in _).join).map(_.flatten)
//    val tcp = (externalDependencyClasspath in Test) <<= (tProjs.map(externalDependencyClasspath in Test in _).join).map(_.flatten)

    val lcp = (libraryDependencies) <<= (sProjs.map(libraryDependencies in _).join).apply(_.flatten)
    val ltcp = (libraryDependencies) <<= (libraryDependencies,(tProjs.map(libraryDependencies in _).join)) { (a : Seq[ModuleID], b : Seq[Seq[ModuleID]]) => a ++ (b.flatten)}

//    val resources = unmanagedResourceDirectories in Test <<= unmanagedResourceDirectories in Test in coreTests
//    val resources = (unmanagedResources in Test) <<= (unmanagedResources in Test in coreTests)

    Project("coverage-proj", file("coverage"), settings = standardSettings ++ dontPublishSettings ++ Seq(									 lcp, ltcp))
		//spaths,tpaths, // cp, tcp,												//	  , resources

  } */

  lazy val fullDocsAndSxr = FullDocs.fullDocsNSources(
    projects = Seq(core, jaxen), projectId = "site",
    projectRoot = file("site"), 
    sxrVersionMap = {
      case v : String if v.startsWith("2.8") =>
	"org.scala-tools.sxr" % "sxr_2.8.0" % "0.2.7"
      case v : String if v.startsWith("2.9") =>
	"org.scala-tools.sxr" % "sxr_2.9.0" % "0.2.7"      
    }, 
    rootProjectId = "scales-xml-root", projectDependencies = Seq(core, jaxen),
    standardSettings = standardSettings ++ Utils.resourceSettings ++ 
      SiteSettings.settings(core) ++ Seq(
	siteCSS <<= siteResourceDir apply { _ / "scales_xml.css" },
	siteMarkupDocs := List("ScalesXmlIntro.mw","MemoryOptimisation.mw"),
	siteMarkupDocHeaders := Map( "ScalesXmlIntro.mw" -> MarkupHeader("An Overview of Scales Xml")("Overview"), "MemoryOptimisation.mw" -> MarkupHeader("An Overview of Memory Optimisation and Performance")("Memory and Performance") ),
	menuBarTitle := "= Scales Xml ${projectVersion} ="
      )
  )

  lazy val dontBuildIn28 = 
    Seq(
      skip <<= scalaVersion map { v => v startsWith "2.8." }, 
      skip in (Compile, update) <<= scalaVersion map { v => v startsWith "2.8." }, 
      skip in (Test, update) <<= scalaVersion map { v => v startsWith "2.8." })

  lazy val dontPublishSettings = Seq(
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    organization := "org.scalesxml.delme"
   )

  // org.scalesxml.delme makes it far easier to deploy by deleting the crap I don't want (https://github.com/harrah/xsbt/issues/484)

  lazy val deployOrg = Seq(
      organization := "org.scalesxml"
    )

  /*lazy val publishSetting = publishTo <<= (version) {
    version: String =>
      val path = "./../repo" +
	(if (version.trim.endsWith("SNAPSHOT"))
	  "-snapshots"
	else "")
      Some(Resolver.file("svn",  new File( path )) )
  }*/

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
/*    shellPrompt := { state =>
 "sbt (%s)$$$-".format(Project.extract(state).currentProject.id)
},
*/
//    organization := "org.scalesxml",
    offline := true,
    version := "0.5-SNAPSHOT",
    scalaVersion := "2.10.0",
//    scalaVersion := "2.10.0-M7",
    crossScalaVersions := Seq("2.8.1", "2.8.2", "2.9.1", "2.9.2","2.10.0"),
    //publishSetting,
//    parallelExecution in Test := false,
    scalacOptions ++= Seq("-optimise"),
//    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    packageOptions ++= Seq[PackageOption](ManifestAttributes(
      (IMPLEMENTATION_TITLE, "Scales"),
      (IMPLEMENTATION_URL, "https://github.com/chris-twiner/scalesXml"),
      (IMPLEMENTATION_VENDOR, "Scales Xml")
      //,(SEALED, "true")
      )
    ),
    /* requires many other command line options and installations for windows:
      -Ddot_exe=%dot_exe%
      * against - no spaces
      set dot_exe=c:/PROGRA~2/GRAPHV~1.28/bin/dot.exe
     */ 
    scalacOptions in (Compile, doc) <++= (scalaVersion).map{(v: String) => 
      if (v.startsWith("2.10"))
	Seq("-diagrams")
      else
	Seq()
    },
    autoCompilerPlugins := false,
    fork in run := true, 
    parallelExecution in runSecurely := false,
    scalaBinaryVersion <<= scalaVersion(sV => if (CrossVersion.isStable(sV)) CrossVersion.binaryScalaVersion(sV) else sV)
//,
//    parallelExecution in jacoco.Config := false
  ) ++ sonatype.settings// ++ jacoco.settings
// ++ crazyness

  val reconPerf = TaskKey[Unit]("recon-perf")

  val filePerf = TaskKey[Unit]("file-perf")

  val runHighPerf = TaskKey[Unit]("run-high-perf")

  val runHighMemory = TaskKey[Unit]("run-high-memory")

  val runPullCollect = TaskKey[Unit]("run-pull-collect")

  val runParseCollect = TaskKey[Unit]("run-parse-collect")

  val runParseCollectRaw = TaskKey[Unit]("run-parse-collect-raw")

  val runPresentation = TaskKey[Unit]("run-presentation")

  val runDeferred = TaskKey[Unit]("run-deferred")

  val runNonDeferred = TaskKey[Unit]("run-non-deferred")

  val runPullCollectLimited = TaskKey[Unit]("run-pull-collect-limited")

  val runHighMemoryFile = InputKey[Unit]("run-high-memory-file")

  val runParseMemory = InputKey[Unit]("run-parse-memory")

  val runMemoryUsage = SettingKey[String]("run-memory-usage")

  /**
   * Due to #1's SecurityException this runs the given task through a security manager,
   * delegating to SBTs (to catch exit etc) that allows configurable security checks.  After the test run it then swaps the security manager back to just sbts.
   */ 
  val runSecurely = TaskKey[Unit]("run-securely")
  val runItSecurely = TaskKey[Unit]("run-it-securely")

  val setSM = TaskKey[TaskStreams]("set-security-manager")

  val semaphore = new java.util.concurrent.Semaphore(1, true)

  def crazyness : Seq[sbt.Project.Setting[_]] = Seq(    
    setSM <<= streams map 
      { (s: TaskStreams) =>
	s.log.info("Setting the security manager")
        //semaphore.acquire
        s
      },
    runItSecurely <<= (setSM, (test in Runtime).task) flatMap { 
	(s, testt) => s.log.info("Running the tests")
	testt },
    runSecurely <<= streams map 
      { (s: TaskStreams) =>
	s.log.info("Resetting the security manager")
	//semaphore.release
      } dependsOn(runItSecurely)
    )

}
