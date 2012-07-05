resolvers += "Scales Repo" at "https://scala-scales.googlecode.com/svn/repo"

libraryDependencies ++= Seq(
  // from our repo
  "org.eclipse.mylyn.wikitext.mediawiki" % "mw.core" % "1.4.0-I20110104-0100-e3x",
  "scales.sbtplugins" % "resources" % "0.1",
  // markdown lib
  "com.tristanhunt" %% "knockoff" % "0.8.0-16",
  "org.jacoco" % "org.jacoco.core" % "0.5.7.201204190339" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
  "org.jacoco" % "org.jacoco.report" % "0.5.7.201204190339" artifacts(Artifact("org.jacoco.report", "jar", "jar"))
)

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")

addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.1")