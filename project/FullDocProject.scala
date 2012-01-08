import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._
import Defaults._

object FullDocs {

  /**
   * Nicked from Scalaz - thanks once again Jason and co.
   * @projects The projects that are packaged in the full distribution.
   */ 
  def fullDocsNSources( projects : Seq[Project], projectId : String, projectRoot : java.io.File, sxrVersionMap : (String) => sbt.ModuleID, rootProjectId : String, projectDependencies : Seq[sbt.ClasspathDep[sbt.ProjectReference]], standardSettings : Seq[sbt.Project.Setting[_]] = Defaults.defaultSettings){

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
      id = projectId,
      base = projectRoot,
      dependencies = projectDependencies,
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
	libraryDependencies <+= scalaVersion{ v => compilerPlugin(sxrVersionMap(v)) },

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        (scaladocOptions in Compile in doc) <++= (baseDirectory, allSourceDirectories) map sxrOptions,

        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject(rootProjectId), baseDirectory, scalaVersion, version,
                docDirectory in Compile, allPackagedArtifacts) map artifactMappings
      )
    )
  }

}
