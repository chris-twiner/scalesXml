import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import Defaults._

import scales.sbtplugins._

object FullDocs {
  
  import Utils._

  lazy val replaceToken = docPathToken
  val siteLinkToken = docPathToken

  val docPathToken = "http://fred.com/docs/"

  val fullDocs = TaskKey[Unit]("full-docs")

  /**
   * Nicked from Scalaz - thanks once again Jason and co.
   * @projects The projects that are packaged in the full distribution.
   */ 
  def fullDocsNSources( projects : Seq[Project], projectId : String, projectRoot : java.io.File, sxrVersionMap : (String) => sbt.ModuleID, rootProjectId : String, projectDependencies : Seq[sbt.ClasspathDep[sbt.ProjectReference]], standardSettings : Seq[sbt.Project.Setting[_]] = Defaults.defaultSettings) = {

    // Some intermediate keys to simplify extracting a task or setting from `projects`.
    val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
    val allSources = TaskKey[Seq[Seq[File]]]("all-sources")
    val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")
    val allTargetDirectories = SettingKey[Seq[File]]("all-target-directories")

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
      id = projectId,
      base = projectRoot,
      dependencies = projectDependencies,
      settings = standardSettings ++ Seq(
        allSources <<= projects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
        allSourceDirectories <<= projects.map(sourceDirectories in Compile in _).join,
        allPackagedArtifacts <<= projects.map(packagedArtifacts in _).join,
	allTargetDirectories <<= projects.map(target in Compile in doc in _).join,

        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (allSources).map(_.flatten),

        // Avoid compiling the sources here; we just are after scaladoc.
        (compile in Compile) := inc.Analysis.Empty,

	// enable SXR for this project only
	autoCompilerPlugins := true,
	libraryDependencies <+= scalaVersion{ v => compilerPlugin(sxrVersionMap(v)) },

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        (scaladocOptions in Compile in doc) <++= (baseDirectory, allSourceDirectories) map sxrOptions,
       	
	/*scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject(projectId)).map {
	  bd => Seq("-sourcepath", projectRoot.getAbsolutePath, "-doc-source-url", docPathToken + "€{FILE_PATH}.scala.html")
	},
	*/
	
	scalacOptions in (Compile, doc) ++= Seq("-doc-source-url", docPathToken + "€{FILE_PATH}.scala.html"),

// can't be allsourcedirs but the target dir itself is needed
	fullDocs in Compile <<= (streams, allSourceDirectories, target in Compile in doc in LocalProject(projectId)) map { 
	  (s, sourceDirectories, target) =>
	  
	  val sxrReplaceWith = "../../api.sxr"

	  val res = repointSxr(target,
	    sourceDirectories.flatten /* source dirs */, 
	    /* this docs dir*/ target ** "*.html", 
	    sxrReplaceWith, 0, replaceToken, s.log)

	  res.foreach{x=>
	    s.log.debug("fullDocs could not be run: "+x)
	    x
	  }

	  s.log.info("fullDocs completed")
	  
	} dependsOn(doc in Compile),
	
        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject(rootProjectId), baseDirectory, scalaVersion, version,
                docDirectory in Compile, allPackagedArtifacts) map artifactMappings
      )
    )
  }

}
