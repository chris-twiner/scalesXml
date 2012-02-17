# Scales Xml

Scales Xml provides both a more flexible approach to XML handling and a simplified way of interacting with XML.  

It fits nicely with the Java APIs you know, TrAX and javax.xml.validation for example, allowing you to run XSLTs and convert to and from Scales XML and other DOMs.

It also provides a far more XPath like experience than the normal Scala XML, Paths look like XPaths and work like them too (with many of the same functions and axes).

A number of the pain points of Scala XML are also simply removed, want to change an attribute?  Just do it on the Element.  Want to match against a namespace, why not?? All name access is fully qualified.  Want to transform all children matching a condition via a Path, that works too.  If sorting the attributes on serializing is of interest to you, plugin a Serializer.

A very flexible XML stream handling approach is based upon StAX and Iteratees (courtesy of Scalaz) which uses the same model as the tree based, no separate event hierarchy needed.

Currently the stable 0.2.1 release [site](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.1/0.2.1/index.html) has focused on performance tuning, including fast parsing and low memory usage (which, of course, are coupled).

The upcoming release 0.3, has an RC4 version and provides ScalesXPath - a string based XPath 1.0 evaluator, a full compliment of useful axe in the internal XPath syntax and general improvements in usability.  

It represents the first xsbt based release and, as such, will have a site after sbtPlugins are migrated. Use "0.3-RC4" as the revision to use this version.

# How To Use

The M2 style repo for snapshots is at https://scala-scales.googlecode.com/svn/repo-snapshots.  Currently 2.8.1 and 2.9.1 are built against.

So for sbt its:

    val scalesRepo = "Scales Repo" at "http://scala-scales.googlecode.com/svn/repo"
    val scalesXml = "scales" %% "scales-xml" % "0.3-RC4"

xsbt 0.10+ its:

    resolvers += "Scales Repo" at "http://scala-scales.googlecode.com/svn/repo"

    libraryDependencies ++= Seq(
      "scales" %% "scales-xml" % "0.3-RC4",
      "scales" %% "scales-jaxen" % "0.3-RC4" // optional for string based xpaths
      )

Maven repos should therefore use scales-xml_2.8.1 as the dependency.

[The last 0.2.1 documentation site is here - 0.3 is pending](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.1/0.2.1/index.html) and zip of the site documentation is available at [scales-xml.zip](http://scala-scales.googlecode.com/svn/sites/scales/scales-xml_2.9.1/0.2.1/scales-xml_2.9.1-0.2.1-site.zip).  

_Warning_ local file based sites do not work in Chrome, use Firefox or IE in preference.

# Mailing List

The Scales dl is: scales-xml at googlegroups.com .  Feel free to ask questions or make suggestions there.  Issues should really be raised on github however.

YourKit has been used to great effect in the creation of scales-xml:

*YourKit* is kindly supporting open source projects with its full-featured Java Profiler.
*YourKit*, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at !YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).