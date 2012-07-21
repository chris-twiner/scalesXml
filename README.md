# Scales Xml

Scales Xml provides both a more flexible approach to XML handling and a simplified way of interacting with XML.  

It fits nicely with the Java APIs you know, TrAX and javax.xml.validation for example, allowing you to run XSLTs and convert to and from Scales XML and other DOMs.

It also provides a far more XPath like experience than the normal Scala XML, Paths look like XPaths and work like them too (with many of the same functions and axes).

A number of the pain points of Scala XML are also simply removed, want to change an attribute?  Just do it on the Element.  Want to match against a namespace, why not?? All name access is fully qualified.  Want to transform all children matching a condition via a Path, that works too.  If sorting the attributes on serializing is of interest to you, plugin a Serializer.

A very flexible XML stream handling approach is based upon StAX and Iteratees (courtesy of Scalaz) which uses the same model as the tree based, no separate event hierarchy needed.

Currently the stable 0.3 release [site](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.2/0.3/index.html) improves on 0.2.1's fast parsing and low memory usage (which, of course, are coupled) and adds ScalesXPath - a string based XPath 1.0 evaluator, a full compliment of useful axe in the internal XPath syntax, an equality framework (2.9.x only) and general improvements in usability.

The artifacts are now on Maven Central under the group org.scales.xml.

# How To Use

Currently 2.8.1, 2.8.2, 2.9.1 and 2.9.2 are built against.

So for sbt its:

    val scalesXml = "org.scalesxml" %% "scales-xml" % "0.3"

xsbt 0.10+ its:

    libraryDependencies ++= Seq(
      "org.scalesxml" %% "scales-xml" % "0.3",
      //"org.scalesxml" %% "scales-jaxen" % "0.3" // optional for string based xpaths
      )

Maven repos should therefore use org.scalesxml scales-xml_2.9.2 as the dependency.   Also note that Jaxen 1.1.4 (tested against 0.3) isn't yet present via Maven, so if you use String XPath evaluation and you'd like to use the latest version you must add the folowing to the build:

    resolvers += "Scales Repo" at "http://scala-scales.googlecode.com/svn/repo"

[The 0.3 documentation site is here](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.2/0.3/index.html) and zip of the site documentation is available at [scales-xml.zip](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.2/0.3/org.scalesxml-scales-xml-0.3-site.zip).

_Warning_ local file based sites do not fully work in Chrome, use Firefox or IE in preference.

# Mailing List

The Scales dl is: scales-xml at googlegroups.com .  Feel free to ask questions or make suggestions there.  Issues should really be raised on github however.

YourKit has been used to great effect in the creation of scales-xml:

*YourKit* is kindly supporting open source projects with its full-featured Java Profiler.
*YourKit*, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at !YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).