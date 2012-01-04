import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._
import Defaults._

object ScalesXmlRoot extends Build {
  lazy val root = Project("scales-xml-root", file("."), settings = standardSettings ++ dontPublishSettings) aggregate(core, coreTests, jaxen, saxonTests, jaxenTests)

  lazy val core = Project("scales-xml", file("core"), settings = standardSettings)

  lazy val coreTests = Project("scales-xml-tests", file("core-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(core)

  lazy val saxonTests = Project("saxon-tests", file("saxon-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(coreTests % "test->test")

  lazy val jaxen = Project("scales-jaxen", file("jaxen"), settings = standardSettings) dependsOn(core)

  lazy val jaxenTests = Project("jaxen-tests", file("jaxen-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(jaxen % "compile->test", coreTests % "test->test") //  % "compile->compile;test->test"

  // Nicked from Scalaz - thanks once again Jason and co.
  lazy val fullDocsAndSxr = {
    // The projects that are packaged in the full distribution.
    val projects = Seq(core, jaxen)

    // Some intermediate keys to simplify extracting a task or setting from `projects`.
    val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
    val allSources = TaskKey[Seq[Seq[File]]]("all-sources")
    val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")

    def artifactMappings(rootBaseDir: File, baseDir: File, scalaVersion: String, version: String,
                         fullDocDir: File, artifacts: Seq[Map[Artifact, File]]): Seq[(File, String)] = {
      val sxrDocDirectory = new File(fullDocDir.getAbsolutePath + ".sxr")

      // Include a root folder in the generated archive.
      val newBase = "scalaz_%s-%s".format(scalaVersion, version)

      val jarsAndPomMappings = artifacts.flatMap(_.values) x flatRebase(newBase)
      val etcMappings = ((rootBaseDir / "etc" ** "*") +++ Seq(rootBaseDir / "README")) x rebase(rootBaseDir, newBase)
      val fullDocMappings = (fullDocDir ** "*") x rebase(fullDocDir.getParentFile, newBase)
      val sxrDocMappings = (sxrDocDirectory ** "*") x rebase(sxrDocDirectory.getParentFile, newBase)
      jarsAndPomMappings ++ etcMappings ++ fullDocMappings ++ sxrDocMappings
    }

    /** Scalac options for SXR */
    def sxrOptions(baseDir: File, sourceDirs: Seq[Seq[File]]): Seq[String] = {
      //val xplugin = "-Xplugin:" + (baseDir / "lib" / "sxr_2.8.0.RC2-0.2.4-SNAPSHOT.jar").asFile.getAbsolutePath
      val baseDirs = sourceDirs.flatten
      val sxrBaseDir = "-P:sxr:base-directory:" + baseDirs.mkString(";").replaceAll("\\\\","/")
      Seq(sxrBaseDir)
    }

    Project(
      id = "scales-full-docs",
      base = file("fullDocs"),
      dependencies = Seq(core, jaxen),
      settings = standardSettings ++ Seq(
        allSources <<= projects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
        allSourceDirectories <<= projects.map(sourceDirectories in Compile in _).join,
        allPackagedArtifacts <<= projects.map(packagedArtifacts in _).join,

        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (allSources).map(_.flatten),

        // Avoid compiling the sources here; we just are after scaladoc.
        (compile in Compile) := inc.Analysis.Empty,

	// enable SXR for this project only
	autoCompilerPlugins := true,
	libraryDependencies <+= scalaVersion{ v =>
	  if (v.startsWith("2.8"))
	    compilerPlugin("org.scala-tools.sxr" % "sxr_2.8.0" % "0.2.7")
	  else 
	    compilerPlugin("org.scala-tools.sxr" % "sxr_2.9.0" % "0.2.7")
					   },

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        (scaladocOptions in Compile in doc) <++= (baseDirectory, allSourceDirectories) map sxrOptions,

        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject("scales-xml-root"), baseDirectory, scalaVersion, version,
                docDirectory in Compile, allPackagedArtifacts) map artifactMappings
      )
    )
  }

  lazy val dontPublishSettings = Seq(
    publishArtifact in (Compile, packageBin) := false,
    publishArtifact in (Compile, packageSrc) := false,
    publishArtifact in (Compile, packageDoc) := false
   )

  lazy val publishSetting = publishTo <<= (version) {
    version: String =>
      val path = "./../repo" +
	(if (version.trim.endsWith("SNAPSHOT"))
	  "-snapshots"
	else "")
      Some(Resolver.file("svn",  new File( path )) )
  }

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
/*    shellPrompt := { state =>
 "sbt (%s)$$$-".format(Project.extract(state).currentProject.id)
},
*/
    organization := "scales",
    offline := true,
    version := "0.3-RC4",
    scalaVersion := "2.9.1",
    crossScalaVersions := Seq("2.8.1", "2.9.1"),
    publishSetting,
//    parallelExecution in Test := false,
    scalacOptions ++= Seq("-optimise"),
//    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    packageOptions ++= Seq[PackageOption](ManifestAttributes(
      (IMPLEMENTATION_TITLE, "Scales"),
      (IMPLEMENTATION_URL, "http://code.google.com/p/scales"),
      (IMPLEMENTATION_VENDOR, "Scales Xml"),
      (SEALED, "true"))
    ),
    autoCompilerPlugins := false,
    fork in run := true,
  //  cpTask,
    ensimeConfig := sexp(
      key(":compiler-args"), sexp("-Ywarn-dead-code", "-Ywarn-shadowing"),
      key(":formatting-prefs"), sexp(
        key(":spaceBeforeColon"), true
      )
    )
  )

  // below is a far finer version than before, ripped from Defaults

  /**
   * At this late stage the properties can be used
   */ 
  def cpRunnerInit(config: sbt.Configuration) : Project.Initialize[Task[ScalaRun]] = 
    (taskTemporaryDirectory, scalaInstance, baseDirectory, javaOptions, outputStrategy, fork, javaHome, trapExit, fullClasspath in config ) map { (tmp, si, base, options, strategy, forkRun, javaHomeDir, trap, cpa) =>
      if(forkRun) {

	val cp = "-classpath" :: Path.makeString(cpa.files) :: Nil 

	new ForkRun( 
	  ForkOptions
	  (scalaJars = si.jars, javaHome = javaHomeDir, outputStrategy = strategy, 
	   runJVMOptions = options ++ cp, 
	   workingDirectory = Some(base)) )
      } else
	new Run(si, trap, tmp)
    }

  def caliperRunTask(scoped: ScopedTask[Unit], config: sbt.Configuration, arguments: String*): Setting[Task[Unit]] =
    scoped <<= ( initScoped(scoped.scopedKey, cpRunnerInit(config)) zipWith (fullClasspath in config, streams).identityMap ) { case (rTask, t) =>
      (t :^: rTask :^: KNil) map { case (cp, s) :+: r :+: HNil =>
	sbt.toError(r.run( "com.google.caliper.Runner", Build.data(cp), 
	  arguments, s.log))
      }
    }

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
				       
}
