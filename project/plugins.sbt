resolvers += "Scales Repo" at "https://scala-scales.googlecode.com/svn/repo"

libraryDependencies ++= Seq(
  // from our repo
  "org.eclipse.mylyn.wikitext.mediawiki" % "mw.core" % "1.4.0-I20110104-0100-e3x",
  "scales.sbtplugins" % "resources" % "0.1"
)
