package org.improving

import sbt._
import Keys._

/*
 You can use this like so:
object YourBuild extends Build {
  object sonatype extends org.improving.PublishToSonatype(YourBuild) {
    def projectUrl    = "https://www.github.com/your/project"
    def developerId   = "your-sonatype-id"
    def developerName = "Your Actual Name"
    // Override more to taste
  }
  
  lazy val yourProject = Project("your-project", file(".")) settings (yourSettings: _*)
  lazy val yourSettings: Seq[Setting[_]] = someSettings ++ sonatype.settings
}

*/

/** After publishing, follow these instructions:
 *  https://docs.sonatype.org/display/Repository/Sonatype+OSS+Maven+Repository+Usage+Guide#SonatypeOSSMavenRepositoryUsageGuide-8.ReleaseIt
 *
 *  Yes, you have to figure out to "close" a repository so that
 *  you can get to the pot of gold.  Perfect choice of word.
 */
abstract class PublishToSonatype(build: Build) {
  import build._

  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
  
  def projectUrl: String
  def developerId: String
  def developerName: String
  
  def licenseName         = "BSD-style"
  def licenseUrl          = "http://www.opensource.org/licenses/bsd-license.php"
  def licenseDistribution = "repo"
  def scmUrl              = projectUrl
  def scmConnection       = "scm:git:" + scmUrl

  def generatePomExtra(scalaVersion: String): xml.NodeSeq = {
    <url>{ projectUrl }</url>
      <licenses>
        <license>
          <name>{ licenseName }</name>
          <url>{ licenseUrl }</url>
          <distribution>{ licenseDistribution }</distribution>
        </license>
      </licenses>
    <scm>
      <url>{ scmUrl }</url>
      <connection>{ scmConnection }</connection>
    </scm>
    <developers>
      <developer>
        <id>{ developerId }</id>
        <name>{ developerName }</name>
      </developer>
    </developers>
  }

  def settings: Seq[Setting[_]] = Seq(
    publishMavenStyle := true,
    publishTo <<= version((v: String) => Some( if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra <<= (scalaVersion)(generatePomExtra)
  )
}
