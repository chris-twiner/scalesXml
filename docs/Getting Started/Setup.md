# Setup

Maven users should use scales-xml_2.12 as the dependency (org.scalesxml as the group).

Scales is cross compiled for 2.11 and 2.12.

## Sbt 0.11.x + 

```scala

libraryDependencies ++= Seq(
  // just for the core library
  "org.scalesxml" %% "scales-xml" % "0.5.0-M1" 
  // and additionally use these for String based XPaths
  "org.scalesxml" %% "scales-jaxen" % "0.5.0-M1" intransitive(),
  "jaxen" % "jaxen" % "1.1.3" intransitive()
  // to use Aalto based parsing
  "org.scalesxml" %% "scales-aalto" % "0.5.0-M1"
)
```

