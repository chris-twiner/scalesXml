import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import Defaults._

object ScalesXmlRoot extends Build {

  lazy val root = Project("scales-xml-root", file("."), settings = standardSettings) aggregate(core, coreTests)

  lazy val core = Project("scales-xml", file("core"), settings = standardSettings)

  lazy val coreTests = Project("scales-xml-tests", file("core-tests"), settings = standardSettings) dependsOn(core)

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalesxml",
    offline := true,
    version := "0.3.1",
    scalaVersion := "2.10.0-M6",
    scalacOptions ++= Seq("-optimise"),
    fork in run := true
  )

}
