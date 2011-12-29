import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._

object ScalesXmlRoot extends Build {
  lazy val root = Project("scales-xml-root", file("."), settings = standardSettings ++ dontPublishSettings) aggregate(core, coreTests, jaxen, saxonTests, jaxenTests)

  lazy val core = Project("scales-xml", file("core"), settings = standardSettings)

  lazy val coreTests = Project("scales-xml-tests", file("core-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(core)

  lazy val saxonTests = Project("saxon-tests", file("saxon-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(coreTests % "test->test")

  lazy val jaxen = Project("scales-jaxen", file("jaxen"), settings = standardSettings) dependsOn(core)

  lazy val jaxenTests = Project("jaxen-tests", file("jaxen-tests"), settings = standardSettings ++ dontPublishSettings) dependsOn(jaxen % "compile->test", coreTests % "test->test") //  % "compile->compile;test->test"

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
    fork in run := true,
    // see notes below
    commands ++= Seq(myRun), 
    cpTask,
    ensimeConfig := sexp(
      key(":compiler-args"), sexp("-Ywarn-dead-code", "-Ywarn-shadowing"),
      key(":formatting-prefs"), sexp(
        key(":spaceBeforeColon"), true
      )
    )
  )

// below from a post on the newsgroup

  // define the classpath task key 
  val cpath = TaskKey[String]("get-classpath", 
                              "Returns the runtime classpath") 

  // define the classpath task 
  val cpTask = cpath <<= fullClasspath.in(
    Runtime) map { (cp: 
		    Classpath) => 
		      cp.files.map(_.getAbsolutePath.replace('\\','/')).mkString(";") 
                } 

  // define the new run command 
  def myRun = Command.args("my-run", "<main to run>") { (state, args) => { 
    // get the results of the classpath task 
    val result: Option[Result[String]] = Project.evaluateTask(cpath, 
							      state) 
    // match based on the results of the task 
    result match { 
      case None => { 
        println("key isn't defined") 
        state.fail 
      } 
      case Some(Inc(inc)) => { 
        println("error: " + inc) 
        // return a failure 
        state.fail 
      } 
      case Some(Value(v)) => { 
        // extract the string from the task results 
        val classpath: String = v 
        // don't know how to set a setting in a command, so just build 
        //  the command that sets it: 
        // javaOptions in run ++= Seq("-cp", classpath) 
        val cmd: String = "set javaOptions in run ++= Seq(\"-cp\", \"" + 
        classpath + 
        "\", \""+ args.mkString("\",\"") + "\")" 
        // return a new state that has the setting command and the run cmd 
        //  (because I don't know how to run an InputTask, just a Task) 
        //   prepended to the list of remaining commands 
        state.copy( 
          remainingCommands = Seq (cmd, "run") ++ state.remainingCommands 
        ) 
      } 
    } 
  } }


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
