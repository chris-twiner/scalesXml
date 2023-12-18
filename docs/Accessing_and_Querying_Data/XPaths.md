# XPath Embedded DSL 

The XML XPath specifications allows navigation of XML documents via a DSL that describes routes through a document using a combination of axe, steps and predicates.  It has a limited number of these abstractions but together they create a powerful direct, whilst remaining simple to use, querying language.

Scales provides this power via both a traditional string based approach and an embedded DSL that leverages the power of Scalas syntactical flexibility to mimic the XPath syntax.

The DSL uses the existing Scales abstractions to the full, and works via a zipper over the XmlTree itself.  Each navigation step through the tree creates new zippers and new paths through the tree.

In every case possible (with the exception of the namespace:: axis) the range of behaviours closely follows the specification, like for like queries matching 100%.  Instead of matching on prefixes Scales uses fully qualified expanded QNames (qualifiedName in the [QName Functions](XPathFunctions.md#qname-functions)) to match against, not requiring a prefix context within which to evaluate.

Internally, perhaps unsurprisingly, XPath is implemented as a combination of filter, map and flatMap.  When retrieving results (e.g. converting to an Iterable) the results are sorted into Document order, this can be expensive for large result sets (see [Unsorted Results](#unsorted-results-and-views) for alternatives).

## Simple Usage Examples

Given the following document:

```scala
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
```

we can easily query for the Subchild:

```scala
  // top produces a Path from a Tree, in this case an XPath
  val path = top(builder)

  val res = path \* ns("Child2") \* ns("Subchild")
  res.size // 1

  string(res) // text
  qname(res) // Subchild
```

## XPath Crash Course

Scales Xml follows the XPath spec fairly closely and accordingly represents the concepts of context, location steps and axe, full details of which can be found in the [XPath Standard](http://www.w3.org/TR/xpath20/#id-path-expressions).

The context, which can be thought of as current "place" in the document, is represented by the following:

* a notional node - where we are in the document - an element, a text node, an attribute etc.
* the position - index within a context and the size of this context

Location steps are a combination of axe, node test and predicates e.g. <nowiki>/*fred</nowiki> which represents the child axe, element node test and a predicate against a no-namespace local name of "fred".

As the XPath adds more axe, steps and predicates the context changes, reducing or expanding possible matches as it develops.  Scales Xml's XPath DSL represents that context with the [XPath](../../site/scaladocs/scales/xml/xpath.XPath.html) class, where each operation on that class returns another immutable instance for the next context.

As with XPath, Scales Xml predicates, axe and node tests can be chained with the current context (the self axe in XPath) always represented by the resulting Scales XPath object.  Only when the underlying results are used (for example by string or qname functions) do they leave the XPath object and get transformed into a, by default, ordered list of matching nodes.

## XPath Axe

Scales supports the complete useful XPath axe, each of which can be used against a given context (an instance of [Scales XPath](../../site/scaladocs/scales/xml/xpath/XPath.html)), for the full XPath axe details find the spec [here](http://www.w3.org/TR/xpath20/#axes):

{|class="genTable"
!XPath Axis
!Scales DSL
!Details
|-
|ancestor||ancestor_::||All the parents of this context
|-
|ancestor-or-self||ancestor_or_self::||All the parents of this context and this node
|-
|attribute||*@||All the attributes for a given context, is often combined directly with a name
|-

|child||\ or \+ to expand XmlItems||Children of this context. NB: \ alone in Scales DSL simply removes the initialNode setting required by \\.  If the children should be expanded (e.g. to use .filter directly) then \+ will "unpack" the child nodes.
|-
|descendant||descendant_::||All children, and their children
|-
|descendant-or-self||descendant_or_self_::||This node and all descendants, also known as \\
|-

|following||following_::||All nodes that follow this context in document order without child nodes of this context
|-
|following-sibling||following_sibling_::||All direct children of this contexts parent node that follow in document order.
|-
|parent||\^||The parent context of this context.  For elements it represents the parent eleemnt and for attributes the containing element.
|-

|preceding||preceding_::||All nodes that precede this context in document order excluding the parent nodes
|-
|preceding-sibling||preceding_sibling_::||All previous children of the parent in the current context in document order.
|-
|self||The XPath object itself via '''.'''||The current context node within a document.
|}

A commonly used abbreviation not listed above is of course \\, which means descendant_or_self_::.  The difference being that \\ also supports possible eager evaluation and as per the spec the notion of [http://www.w3.org/TR/xpath20/#id-path-expressions \\ in the beginning expression].

''NB Scales Embedded XPath DSL does not support the namespace axis - if you have a requirement for it then it can be looked at (please send an email to [mailto:scales-xml@googlegroups.com the mailing list] to discuss possible improvements)''

## Node Tests

Scales embedded XPath DSL views the majority of node tests as predicates

{|class="genTable"
!XPath Node Test
!Scales DSL
!Details
|-
|node()||.\+||Returns a new context for all the children below a given context
|-
|text()||.text||Returns a new context for all the text and cdata below a given context
|-
|comment()||.comment||Returns a new context for all the comments below a given context
|}

Scales XML also adds:

* .textOnly - filters out CData, just giving text nodes
* .cdata - provides CData nodes
* .pi - provides processing instructions

## Predicates

There are three areas allowing for predicates within XPaths:

* Attributes
* Elements
* General

The first two are special cased, as in the XPath spec, as they are the most heavily used predicates (using the above example document):

```scala
  // QName based match
  val attributeNamePredicates = path \@ nsp("attr3")
  string(attributeNamePredicates) // "val3"
  
  // predicate based match
  val attributePredicates = path \@ ( string(_) == "val3" )
  qualifiedName(attributePredicates) // {test:uri:attribs}attr3

  // Find child descendants that contain a Subchild 
  val elemsWithASubchild = path \\* ( _ \* ns("Subchild"))
  string(elemsWithASubchild) // text
  qualifiedName(elemsWithASubchild) // {test:uri}Child2
```

In each case the XmlPath (or AttributePath) is passed to the predicate with a number of shortcuts for the common QName based matches and positional matches for elements:

```scala
  val second = path \*(2) // path \* 2 is also valid but doesn't read like \*[2]
  qname(second) // Child2
```

The developer can chose to ignore namespaces (not recommended) by using the *:* and *:@ predicates instead (equivalent to string xpath /*[local-name() = "x"]).

### Predicate Construction

All the predicates in Scales are built from two simple building blocks:

1. XmlPath => Boolean - via the XPath.filter function
2. AttributePath => Boolean - via the [AttributeAxis](../../site/scaladocs/scales/xml/xpath/AttributeAxis.html).*@ function

The various base node types and filters are based on these functions, for example the element predicate * is implemented as:

```scala
def *(pred : XmlPath => Boolean) : XPath[T] = 
  filter(x => x.isItem == false && pred(x))
```

In turn \* can be seen as a combination of the \ child step and the * predicate (via xflatMap) and is provided as syntactic sugar.

Similarly text is implemented using filter.

All of the standard set of predicates (and axis combinations) can be found in the  [XPath](../../site/scaladocs/scales/xml/xpath/XPath.html) ScalaDoc.  Clicking the right arrow for many of the functions will lead you to the Definition Classes docs and their code. 

### Chaining Predicates

Predicates can be chained on the context itself, i.e. the XPath object, for example:

```scala
val pathsCombinedPredicates =
    root.\*(ns("Child")).
      *(_.\@( nsp("attr3") )) // context is still Child matches, but has additionally reduced it to only items with an attribute of attr3
```

This represents <nowiki>/root/*ns:Child[.\@nsp:attr3]</nowiki> where the * Scales Xml element predicate allows matching on the self axis.  The same chaining is available on the attribute axis represented by the [./doc/scales/xml/xpath/AttributePaths.html AttributePaths] class.

### Positional Predicates

{|class="genTable"
!XPath Position Function
!Scales DSL
!Details
|-
|position()||pos_<, pos_==, pos() and pos_>||Functions to work against the current position within a context
|-
|last()||last_<, last_== and last_>||Functions that work against the size of a given context
|-
|position() == last()||pos_eq_last||Take the last item in a context
|}

These, more difficult to model, positional tests can be leveraged the same way as position() and last() can be in XPath.

So, for example:

```scala
  // /*[position() = last()]
  val theLast = path.\.pos_eq_last
  qname(theLast) // Elem

  // //*[position() = last()]
  val allLasts = path.\\*.pos_eq_last
  allLasts map(qname(_)) // List(Elem, Child2, Subchild)

  // all elems with more than one child
  // //*[ ./*[last() > 1]]
  val moreThanOne = path.\\*( _.\*.last_>(1) )
  qname(moreThanOne) // Elem

  // all elems that aren't the first child
  // //*[ position() > 1]
  val notFirst = path.\\*.pos_>(1)
  qname(notFirst) // Child2
```

### Direct Filtering

The xflatMap, xmap, xfilter and filter methods allow extra predicate usage where the existing XPath 1.0 functions don't suffice.

The filter method accepts a simple XmlPath => Boolean, whereas the other varieties work on the matching sets themselves.

It is not recommended to use these functions for general use as they primarily exist for internal re-use.

## Unsorted Results and Views

In order to meet XPath expected usage results are sorted in Document order and checked for duplicates.  If this is not necessary - but speed of matching over a result set is (for example lazy querying over a large set) - then the raw functions (either raw or rawLazy) are good choices.

The viewed function however uses views as its default type and may help add further lazy evaluation.  Whilst tests have shown lazy evaluation takes place its worth profiling your application to see if it actually impacts performance in an expected fashion.

See the [XmlPaths trait](../../site/scaladocs/scales/xml/xpath/XmlPaths.html) for more information.
