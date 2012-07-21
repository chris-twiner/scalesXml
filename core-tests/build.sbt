import scales.sbtplugins.Utils._

resolvers += "Sonatype OSS Repo" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-log4j12" % "1.6.1" % "test",	
  "com.novocode" % "junit-interface" % "0.8" % "test",
  "com.google.code.java-allocation-instrumenter" %
    "java-allocation-instrumenter" % "2.0" % "test",
  // needs to be overriden  
  "com.google.code.gson" % "gson" % "1.7.2" % "test",
  "com.google.caliper" % "caliper" % "0.5-rc1" % "test",
  "nu.validator.htmlparser" % "htmlparser" % "1.4",
  "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
)

excludeFilter in unmanagedSources <<= scalaVersion{ v => 
  if (v.startsWith("2.8")) 
     "*_2.9.scala"
   else 
     "*_2.8.scala"}

caliperRunTask(reconPerf, Test, "scales.xml.ParsingPerformanceRecon", "-JmaxMem=-Xmx256M") // 256

fork in reconPerf := true


caliperRunTask(filePerf, Test, "scales.xml.SpecificFilePerformance")

fork in filePerf := true


fullRunTask(runHighMemory, Test, "scales.xml.RunMemoryOptimised")

fullRunTask(runHighPerf, Test, "scales.xml.RunHighPerf")

javaOptions in runHighPerf += "-Xmx135M"

fullRunTask(runPullCollect, Test, "scales.xml.RunPullCollect")

fullRunTask(runParseCollect, Test, "scales.xml.RunParseCollect")

fullRunTask(runParseCollectRaw, Test, "scales.xml.RunParseCollectRaw")

fullRunTask(runPresentation, Test, "scales.xml.Presentation")

fullRunTask(runDeferred, Test, "scales.xml.RunDeferred")

javaOptions in runDeferred += "-Xmx160M"

fullRunTask(runNonDeferred, Test, "scales.xml.RunNonDeferred")

javaOptions in runNonDeferred += "-Xmx135M"

fullRunTask(runPullCollectLimited, Test, "scales.xml.RunPullCollect")

javaOptions in runPullCollectLimited += "-Xmx45M"

fullRunInputTask(runHighMemoryFile, Test, "scales.xml.RunMemoryOptimisedFile")


  
fullRunInputTask(runParseMemory, Test, "scales.xml.RunParseMemory")

fork in runParseMemory := true

runMemoryUsage := "-Xmx120M"

// change in console via set runMemoryUsage in coreTests := "-Xmx40M"
javaOptions in runParseMemory <+= runMemoryUsage// map "-Xmx45M"
