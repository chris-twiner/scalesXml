= XPath Functions =

The XPath specs define a number of useful functions which are specific to XML, other string or number related functionality is provided by Scala itself and is not replicated by the XPath DSL.

== Organisation ==

The XPath Functions are organised into two main type classes:

# Names
# TextValue
# Boolean

Names provides a type class for any type that can represent a QName:

* Attributes
* Elems
* XmlTrees
* XPaths (name of the first node, "empty" otherwise)
* QNames

TextValue, representing everything that can produce a text value, has a similar base list:

* XmlTrees
* Attributes
* XmlItems
* XPaths (value of the first node)

In general if any logical combination thereof is possible they are also supported (for example XmlPaths themselves)

The boolean family of type class instances:

* XPaths (is it non-empty)
* Iterables (is it non-empty)
* String (length > 0)
* Number (value > 0)
and of course
* Boolean

== QName Functions ==

The full list of functions is available [doc/scales/xml/xpath/NameFunctions.html here].

Some examples:

${cscala}
  val attr : Attribute = "attr" -> "value"

  qname(attr) // attr
  pqName(attr) // {}attr
  namespaceUri(attr) // ""

  val ns = Namespace("uri:namespace")
  val pre = ns.prefixed("pre")

  val prefixedAttr : Attribute = pre("prefixed") -> "prefixed value"
  
  qname(prefixedAttr) // pre:prefixed
  pqName(prefixedAttr) // pre:{uri:namespace}prefixed
  namespaceUri(prefixedAttr) // uri:namespace

  val elem = Elem(pre("prefixedElem"))
  
  qname(elem) // pre:prefixedElem
  pqName(elem) // pre:{uri:namespace}prefixedElem
  namespaceUri(elem) // uri:namespace

  val tree = elem /( elem ~> "\ndeep  value\n" )

  pqName(tree) // pre:{uri:namespace}prefixedElem
${cend}

Using the `name` function with a non QName XPath (e.g. an XmlItem) will result in an exception.

== Text Functions ==

The full list of functions is available [doc/scales/xml/xpath/TextFunctions.html here].

Some examples (using the above definitions):

${cscala}
  value(attr) // value
  value(prefixedAttr) // prefixed value

  // won't compile as there is no meaningful way to get an elems value
  // value(elem)

  value(tree.toTree) // \ndeep  value\n
  normalizeSpace(tree) // deep value, but using type class directly
${cend}

== Boolean Function ==

${cscala}
  boolean("") // false
  boolean("value") // true

  boolean(true) // true
${cend}