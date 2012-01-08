import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._
import Defaults._

object Utils {

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

  /**
   * Runner that can run caliper tasks or any others requiring the classpath be specified on the forked app
   */ 
  def caliperRunTask(scoped: ScopedTask[Unit], config: sbt.Configuration, arguments: String*): Setting[Task[Unit]] =
    scoped <<= ( initScoped(scoped.scopedKey, cpRunnerInit(config)) zipWith (fullClasspath in config, streams).identityMap ) { case (rTask, t) =>
      (t :^: rTask :^: KNil) map { case (cp, s) :+: r :+: HNil =>
	sbt.toError(r.run( "com.google.caliper.Runner", Build.data(cp), 
	  arguments, s.log))
      }
    }

}
