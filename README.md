# Scales Xml

[![Build Status](https://travis-ci.org/chris-twiner/scalesXml.png)](https://travis-ci.org/chris-twiner/scalesXml)

Scales Xml provides both a more flexible approach to XML handling and a simplified way of interacting with XML.  

It fits nicely with the Java APIs you know, TrAX and javax.xml.validation for example, allowing you to run XSLTs and convert to and from Scales XML and other DOMs.

It also provides a far more XPath like experience than the normal Scala XML, Paths look like XPaths and work like them too (with many of the same functions and axes).

A number of the pain points of Scala XML are also simply removed, want to change an attribute?  Just do it on the Element.  Want to match against a namespace, why not?? All name access is fully qualified.  Want to transform all children matching a condition via a Path, that works too.  If sorting the attributes on serializing is of interest to you, plugin a Serializer.

A very flexible XML stream handling approach is based upon StAX and Iteratees (courtesy of Scalaz) which uses the same model as the tree based, no separate event hierarchy needed.

Currently the stable 0.6.0 release [site](https://chris-twiner.github.io/scalesXml/) improves on 0.2.1's fast parsing and low memory usage (which, of course, are coupled) and adds ScalesXPath - a string based XPath 1.0 evaluator, a full compliment of useful axe in the internal XPath syntax, an equality framework (2.9.x only) and general improvements in usability.

The artifacts are now on Maven Central under the group org.scales.xml.

# How To Use

Currently, 2.11, 2.12 and 2.13 are built against.

So for sbt its:

    val scalesXml = "org.scalesxml" %% "scales-xml" % "0.6.0-M4"

xsbt 0.10+ its:

    libraryDependencies ++= Seq(
      "org.scalesxml" %% "scales-xml" % "0.6.0-M4",
      "org.scalesxml" %% "scales-xml-jaxen" % "0.6.0-M4", // optional for string based xpaths
      "org.scalesxml" %% "scales-xml-aalto" % "0.6.0-M4" // optional for string based xpaths
      )

Maven repos should therefore use org.scalesxml scales-xml_2.13 as the dependency.

# Mailing List

The Scales dl is: scales-xml at googlegroups.com .  Feel free to ask questions or make suggestions there.  Issues should really be raised on github however.

YourKit has been used to great effect in the creation of scales-xml:

*YourKit* is kindly supporting open source projects with its full-featured Java Profiler.
*YourKit*, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at !YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).
