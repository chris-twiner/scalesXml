= XPath 1.0 String Evaluation =

The embedded XPath DSL provides a very flexible and performant way to query xml.  However, being an embedded DSL, it has the negative of being wed to the compilation cycle.  For those seeking a more direct XPath string parsing based experience Scales provides access via the Jaxen project.

The scales-jaxen project is a sub project and must be included separately as a library dependency.

== How To Use ==

The following dependency must be used (instead of scales-xml):

${cscala}
  "scales" %% "scales-jaxen" % "0.3" // or 0.4 for a Scalaz 6.0.4 dependency
${cend}

After importing the package use the ScalesXPath constructor with either a map of prefix to uri, or a list of PrefixedNamespaces (similar to Elem construction):

${cscala}
  import scales.xml.jaxen._

  // prefix mappings are needed for context
  val aPath = ScalesXPath("//*[1]", Map("pre" -> "urn:prefix"))
  // the type annotation is for illustrative reasons only :->
  val result : Iterable[Either[AttributePath, XmlPath]] = aPath.evaluate(anXmlPath)

  // direct XPath results use get
  val anotherPath = ScalesXPath("string(//*[1])", Map("pre" -> "urn:prefix"))
  // This uses casts to retrieve values, it can throw.
  val string = anotherPath.get[String](anXmlPath)
${cend}

Results are always returned in Document order.

Rather than dealing with an Either for results, when the developer probably knows what type he wants, xmlPaths and attributePaths can be used:

${cscala}
  val ns = Namespace("test:uri")
  val nsa = Namespace("test:uri:attribs")
  val nsp = nsa.prefixed("pre")

  val builder = 
    ns("Elem") /@ (nsa("pre", "attr1") -> "val1",
      	    	   "attr2" -> "val2",
		   nsp("attr3") -> "val3") /(
      ns("Child"),
      "Mixed Content",
      ns("Child2") /( ns("Subchild") ~> "text" )
    )

  val elems = ScalesXPath("//*")
  elems.xmlPaths(path).map(qname(_)) // List(Elem, Child, Child2, Subchild)
  elems.attributePaths(path).map(qname(_)) // List()

  val attribs = ScalesXPath("//@*")
  attribs.xmlPaths(path).map(qname(_)) // List()
  attribs.attributePaths(path).map(qname(_)) // List(pre:attr3, attr2, pre:attr1)
${cend}

As a shortcut to ignore namespaces (instead of using prefixes) use:

${cscala}
  ScalesXPath("/html/body/p[2]/table[2]/tr/td/table/tr/td[1]/a/font").withNameConversion(ScalesXPath.localOnly)
${cend}

The function passed to withNameConversion is of type: 

${cscala}QName => QName${cend} 

allowing further mappings as needed.

== Other Jaxen Tricks ==

Jaxen provides various extensions to straight forward querying, including adding java extension functions.  If possible, of course, use the embedded DSL, if not ScalesXPath is a Jaxen XPath implementation, allowing calls to setFunctionContext, variableContext etc.