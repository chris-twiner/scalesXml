//resolvers += "Sonatype OSS Repo" at "http://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.4",
  "org.slf4j" % "slf4j-api" % "1.6.1"
//,
  //"commons-jxpath" % "commons-jxpath" % "1.3"
)

libraryDependencies <++= scalaVersion{ v =>
      Seq("org.scalaz" %% "scalaz-core" % "7.0.6",
//      "org.scalaz" %% "scalaz-iteratee" % "7.0.6",
      "org.scalaz" %% "scalaz-iterv" % "7.0.6" )
}

excludeFilter in unmanagedSources <<= scalaVersion{ v => 
  if (v.startsWith("2.8")) 
     "*_2.9.scala"
   else 
     "*_2.8.scala"}

// libraryDependencies <+= scalaVersion{ v=>
//    if (v.startsWith("2.8")) // stick to 2.8.1
//      "org.scalaz" % "scalaz-core_2.8.1" % "6.0.3"
//    else
//      "org.scalaz" %% "scalaz-core" % "6.0.3" } */
