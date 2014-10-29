# Scales Xml

[![Build Status](https://travis-ci.org/chris-twiner/scalesXml.png)](https://travis-ci.org/chris-twiner/scalesXml)

Scales Xml provides both a more flexible approach to XML handling and a simplified way of interacting with XML.  

It fits nicely with the Java APIs you know, TrAX and javax.xml.validation for example, allowing you to run XSLTs and convert to and from Scales XML and other DOMs.

It also provides a far more XPath like experience than the normal Scala XML, Paths look like XPaths and work like them too (with many of the same functions and axes).

A number of the pain points of Scala XML are also simply removed, want to change an attribute?  Just do it on the Element.  Want to match against a namespace, why not?? All name access is fully qualified.  Want to transform all children matching a condition via a Path, that works too.  If sorting the attributes on serializing is of interest to you, plugin a Serializer.

A very flexible XML stream handling approach is based upon StAX and Iteratees (courtesy of Scalaz) which uses the same model as the tree based, no separate event hierarchy needed.  New in 0.5.0 is also support for asynchronous pull parsing - no more blocking on streams.

The current stable 0.5.0 release [site](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.10/0.5.0/index.html) improves again on Scales's fast parsing and low memory usage (which, of course, are coupled) and adds optional trees to a number of other usability improvements.

The artifacts are now on Maven Central under the group org.scales.xml.

# How To Use

Scales 0.5.0 is built against 2.9.3 and 2.10.

So for sbt its:

    val scalesXml = "org.scalesxml" %% "scales-xml" % "0.5.0"

xsbt 0.10+ its:

    libraryDependencies ++= Seq(
      // just for the core library
      "org.scalesxml" %% "scales-xml" % "0.5.0",
      // and additionally use these for String based XPaths
      "org.scalesxml" %% "scales-jaxen" % "0.5.0" intransitive(),
      "jaxen" % "jaxen" % "1.1.3" intransitive(),
      // to use Aalto based parsing
      "org.scalesxml" %% "scales-aalto" % "0.5.0"
    )

intransitive() is required because the Jaxen pom cannot be fully used.

Maven repos should therefore use org.scalesxml scales-xml_2.10 as the dependency.   Also note that Jaxen 1.1.4 isn't yet present via Maven, so if you use String XPath evaluation and you'd like to use the latest version you must add the folowing to the build:

    resolvers += "Scales Repo" at "http://scala-scales.googlecode.com/svn/repo"

[The 0.5.0 documentation site is here](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.10/0.5.0/index.html) and zip of the site documentation is available at [scales-xml.zip](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.10/0.5.0/org.scalesxml-scales-xml-0.5.0-site.zip).

_Warning_ local file based offline sites may not fully work in Chrome, use Firefox or IE as needed.

# Mailing List

The Scales dl is: scales-xml at googlegroups.com .  Feel free to ask questions or make suggestions there.  Issues should really be raised on github however.

YourKit has been used to great effect in the creation of scales-xml:

*YourKit* is kindly supporting open source projects with its full-featured Java Profiler.
*YourKit*, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at !YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).
