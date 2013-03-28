= Setup =

Maven users should use scales-xml_2.9.2 as the dependency (org.scalesxml as the group).

The 0.5.0-M1 documentation site is [http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.10/0.5.0-M1/index.html here] and a zip of the site documentation is available at [http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.10/0.5.0-M1/org.scalesxml-scales-xml-0.5.0-M1-site.zip scales-xml.zip].

Scales is cross compiled for 2.8.1, 2.8.2, 2.9.1, 2.9.2 and 2.10.

== Sbt 0.11.x + ==

${cscala}

libraryDependencies ++= Seq(
  // just for the core library
  "org.scalesxml" %% "scales-xml" % "0.5.0-M1" 
  // and additionally use these for String based XPaths
  "org.scalesxml" %% "scales-jaxen" % "0.5.0-M1" intransitive(),
  "jaxen" % "jaxen" % "1.1.3" intransitive()
  // to use Aalto based parsing
  "org.scalesxml" %% "scales-aalto" % "0.5.0-M1"
)
${cend}

