package scales.sbtplugins

import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import org.ensime.sbt.Plugin.Settings.ensimeConfig
import org.ensime.sbt.util.SExp._
import Defaults._

import java.io.File

import IO._
import Path._

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

// provided by IO now
//  lazy val utf8 = java.nio.charset.Charset.forName("UTF-8")

  /**
   * I don't find the orElse helps with reading ~> seems to indicate flow more
   */ 
  implicit def toSquiglyOrElseAndCata[A]( option : => Option[A] ) = new {
    // cache it, just evaluating the option when creating forces it twice when used with ~~>
    lazy val eoption = option
    def ~~>[B >: A](alternative : => Option[B]) : Option[B] = eoption.orElse(alternative)
    def cata[B]( whenFull : (A) => B, whenEmpty : => B) = 
      if (eoption.isDefined) 
	whenFull(eoption.get)
      else
	whenEmpty
  }

  /**
   * For forcing token evaluation
   */ 
  implicit def evalVals( orig : Map[String, ()=>String] ) : Map[String, String] = Map[String,String]() ++ orig.map((x) => (x._1,x._2()))

  /**
   * doesn't force the original not to exist.
   */
  def copyDir(source: File, target: File, filterOut : Iterable[FileFilter], log: Logger): Option[String] = copyDir(source, target, filterOut.foldLeft(NothingFilter:FileFilter){_ || _}, log)

  def ioCatching( t : => Option[String])(log : Logger ) : Option[String] = {
    try {
      t
    } catch {
      case e => log.error("Could not perform io because of " + e.getMessage)
	Some(e.getMessage)
    } 
  }

  def ioCatchingRes[T]( t : => T)(log : Logger ) : Either[String, T] = {
    try {
      Right(t)
    } catch {
      case e => log.error("Could not perform io because of " + e.getMessage)
	Left(e.getMessage)
    } 
  }

  def createDirectory(source : File, log : Logger) : Option[String] =
    ioCatching{
      IO.createDirectory(source)
      if (source.exists)
	None
      else
	Some("Could not create direct for an unknown reason "+source.getAbsolutePath)
    }(log)

  def copyFile(source: File, target : File, log: Logger) : Option[String] =
    ioCatching{
      IO.copyFile(source, target)
      None
    }(log)

  /**
   * doesn't force the original not to exist.
   */
  def copyDir(source: File, target: File, filterOut : FileFilter, log: Logger): Option[String] = {
    require(source.isDirectory, "Source '" + source.getAbsolutePath + "' is not a directory.")

    def copyDirectory(sourceDir: File, targetDir: File): Option[String] =
      if (filterOut.accept(sourceDir))
	None
      else
	createDirectory(targetDir, log) ~~> copyContents(sourceDir, targetDir)

    def copyContents(sourceDir: File, targetDir: File): Option[String] =
      sourceDir.listFiles.foldLeft(None: Option[String]) {
      (result, file) =>
	result ~~> {
	  val targetFile = new File(targetDir, file.getName)
	  if(file.isDirectory)
	    copyDirectory(file, targetFile)
	  else {
	    if (filterOut.accept(targetFile))
	      None
	    else
	      copyFile(file, targetFile, log)
	  }
	}
      }

    copyDirectory(source, target)
  }

  def copyFiles( dir : File , files : Iterable[String], to : File, log : Logger ) : Option[String] = 
    files.foldLeft(None : Option[String]){
      (x,y) =>
	x ~~> {
	  copyFile(dir / y, to / y, log)
	}
    }

  def toForwards( str : String ) = str.replaceAll("\\" + java.io.File.separator, "/")

  def depth( str : String ) = {
    def idepth(str : String,  pos : Int, acc: Int ) : Int = {
      val npos = str.indexOf( '/', pos + 1)
      if (npos < 0) acc
      else idepth(str, npos, acc + 1)
    }
      
    val start = toForwards(str)
    idepth(start, 0, 0)
  }

  def captureErrors( itr : Iterable[Option[String]]) = 
    itr.foldLeft(None : Option[String]){ 
      (x, y) => 
      if (y.isDefined) x.map(_ + y).orElse(y)
      else x
    }
  
  /**
   * The either returns a good result for Right or an error for left
   */ 
  def manip( path : File, log: Logger ) ( f : String => Either[String,String]) =
    ioCatchingRes(IO.read(path.asFile, utf8))(log).
    fold( Some(_) , str =>
      ioCatching(
	f(str).fold( Some(_) , x => {
	  write(path.asFile, x, utf8);None} )
      )(log)
    )

  /**
   * Replaces replaceToken in all files identified by paths with replaceWith prefixed by a number of "../".
   * This number is based on the relative depth of basePaths' files against path + depthAdjustment.
   * In a multiproject setup each fullDoc project list should have its path provided
   */ 
  def repointSxr( base : File, sourcePaths : Iterable[File], paths : PathFinder, replaceWith : String, depthAdjustment : Int, replaceToken : String, log : Logger ) = 
    captureErrors(paths.get.map{ x => 
      IO.relativize( base, x).map {
	rp =>

	// for the package depth replace the generated root with ""
	val d = depth(rp)

	val dots = (1 to (d + depthAdjustment - 1)).foldLeft(""){(x, y) => x + "../"}

	manip(x,log){
	  str =>	
	  sourcePaths.foldLeft(None : Option[String]){ 
	    (current, y) => 
	      current ~~> {// go until one matches
		val rep = replaceToken + toForwards(y.getAbsolutePath)
		val pos = str.indexOf(rep)

		if (pos > -1) { // don't like doing it twice
		  val sb = new java.lang.StringBuilder()
		  sb.append(str.substring(0,pos))
		  sb.append(dots+replaceWith)
		  
		  // is it a java bit? copy up to the next bit
		  val quote = str.indexOf("\"", pos)
		  val bjava = str.indexOf(".java.scala.html", pos)
		  if (bjava > -1 && bjava < quote) {
		    sb.append(str.substring(pos + rep.length, bjava))
		    sb.append(".java")
		    sb.append(str.substring(bjava + 11))
		  } else {
		    sb.append(str.substring(pos + rep.length))
		  }

		  //val newStr = str.replace(rep , dots + replaceWith)
		  // does it have a java bit on the end? .java.scala.html" is a bit funny
		  

		  Some(sb.toString)
		} else {
		  log.debug("Didn't find "+ rep+ "in file "+x.getAbsolutePath)
		  None
		}
	    }
	  }.map{ Right(_) }.
	    getOrElse(Left("Couldn't find a source path to replace for file "+x.getAbsolutePath))
	}
      }
    }.flatten)

  /**
   * Commands split by ; use spaces in commands to pass params to calls.  No spaces calls an action.  they don't work quite like that in xsbt land.
   *
  def callCommands( cmds : String, project : Project, canFail : List[String]) = 
    cmds.split(";").foldLeft(None: Option[String]){ (x,y) => 
      x ~~> {
	val command = y.trim
	val (res, cmd) = if (command.isEmpty) (x,command)
	else if (command.indexOf(' ') > -1){
	  val bits = command.split(' ').toList
	  (project.call(bits.head, bits.tail.toArray),
	    bits.head)
	} else 
	  (project.act(command), command)
	
	if (canFail.contains(cmd))
	    None
	else {
	  res.flatMap{ x=> 
		  if (x.matches("(Action|Method) '.*' does not exist.")) {
		    project.log.warn("Can't run siteAllAction fully: "+x+" <-- Check the siteAllAction for non valid tasks or commands.")
		    None
		  } else Some(x) }
	}
      }
    }*/
  
  /**
   * returns a css link for this path
   */ 
  def css(path : String) = "<link href=\""+path+"\" rel=\"stylesheet\" type=\"text/css\" media=\"screen\" />"

  /**
   * return js
   */ 
  def js(path : String) = "<script src=\""+path+"\" type=\"text/javascript\"></script>"

  /**
   * default base css
   */ 
  val baseCss = SettingKey[File]("site.baseCss")

  /**
   * Default location for the expanded plugin resources
   */
  val resourcesOutDir = SettingKey[File]("site.resourcesOutDir")

  val unpackResourcesTask = TaskKey[Option[String]]("site-unpackResources")

  def resourceSettings = Seq(
    resourcesOutDir <<= (resourceManaged in compile) apply { _ / resourcesName },
    baseCss <<= (resourcesOutDir) apply { _ / "scales_base.css" },
    unpackResourcesTask <<= (resourcesOutDir, streams) map { (d, s) =>
      unpackResources(d, s.log)
    } 
  )

  /**
   */
  def title(tit : String) = "<title>"+tit+"</title>"

  val resourcesName = "scales.sbtplugins.resources"

  /**
   * Unpacks the resources from the jar into the temporary resources dir
   *
   * Doesn't unpack if the directory is there
   */
  def unpackResources( to : File, logger : Logger ) : Option[String] = {
    if (!to.exists) {
      createDirectory(to, logger)
      val cl = this.getClass.getClassLoader
      val stream = cl.getResourceAsStream(resourcesName+"/filelist.txt")
      if (stream eq null) Some("Could not find filelist.txt, cannot unpack to "+to.getAbsolutePath)
      else {
	val res = scala.io.Source.fromInputStream(stream).getLines.foldLeft(None : Option[String]){ (x , oline ) =>
	  val line = oline.trim
	  x ~~> {
	    val fs = cl.getResourceAsStream(resourcesName+"/"+line)
	    if (fs eq null) Some("Could not open "+line)
	    else
	      ioCatching{
		val tos = getOStream(to / line asFile)
		tos.map{ toss =>
		  transferAndClose(fs, toss)
		  
		  None
		}.getOrElse(Some("Could not open the file "+(to / line asFile)))
	      }(logger)
	  }
	}
	stream.close()
	res
      }
    } else {
      logger.debug("To directory already exists (already unpacked) : "+to.getAbsolutePath)
      None
    }
  }

  def getStream( f : File ) : Option[java.io.FileInputStream] = 
    try{
      Some(new java.io.FileInputStream(f))
    } catch {
      case e => None
    }

  def getOStream( f : File ) : Option[java.io.FileOutputStream] = 
    try{
      Some(new java.io.FileOutputStream(f))
    } catch {
      case e => None
    }
}
