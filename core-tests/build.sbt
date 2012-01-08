import Utils._

resolvers += "Sonatype OSS Repo" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-log4j12" % "1.6.1" % "test",	
  "com.novocode" % "junit-interface" % "0.7" % "test",
  "com.google.code.java-allocation-instrumenter" %
    "java-allocation-instrumenter" % "2.0" % "test",
  // needs to be overriden  
  "com.google.code.gson" % "gson" % "1.7.2" % "test",
  "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" % "test"
)

caliperRunTask(reconPerf, Test, "scales.xml.ParsingPerformanceRecon", "-JmaxMem=-Xmx256M")

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


  
