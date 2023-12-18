# QNames

[http://www.w3.org/TR/2009/REC-xml-names-20091208/#NT-NCName The namespaces spec] introduced namespaces into XML, giving the three following possible types of qualified names:

1. Fully Qualified (local name, prefix and namespace)
2. No namespace (local name only xmlns="")
3. Namespaced (local name and namespace - xmlns="a new default scoped namespace")

This can be seen modelled in the JAXP QName class, or as the QName ADT in Scales.  The namespaces spec introduced the letter ":" as a separator of prefix and local name, as such its use is forbidden in QName prefixes or localNames (runtime check).

## Creating QNames

### Directly

```scala
  val ns = Namespace("uri:namespace")
  val pre = ns.prefixed("pre")
  val prefixed = PrefixedQName( "local", pre )

  // same as above
  val prefixed2 = pre("local")

  // without a prefix - only for Elems
  val namespaced = ns("local")

  val noNamespace = NoNamespaceQName("local")

  val prefixedDirect = ns.prefixed("pre2","local")
```

### Implicits

```scala
  // implicitly make it, allowing for \*("localNameOnly") XPath 1.0 syntax
  val localOnly : NoNamespaceQName = "local"

  // "directly" declaring it local with .l
  val noNamespace = "local"l

  val unprefixed = "test:namespace"::"local"
```

## Namespaces & Scope

Namespaces are scoped according to the nesting of XML elements.  If the root declares a default namespace it applies for all of the XML child elements unless overridden by introducing a new scoped default namespace.

This scoping does not apply to attributes, they are either fully qualified or have namespace xmlns="".  Namespaces are in XML 1.0 URIs and in XML 1.1 IRIs.

Namespace prefixes also have a different binding relationship in XML 1.1 documents, where xmlns:pre="" is actually legal (albeit very confusing) and unbinds the "pre" prefix.  This makes it invalid to use the "pre" prefix again (taken from the 1.1 namespaces spec):

```xml
<?xml version="1.1"?>
<x xmlns:n1="http://www.w3.org">
    <n1:a/>               <!-- legal; the prefix n1 is bound to http://www.w3.org -->
    <x xmlns:n1="">
        <n1:a/>           <!-- illegal; the prefix n1 is not bound here -->
	<x xmlns:n1="http://www.w3.org">
            <n1:a/>       <!-- legal; the prefix n1 is bound again -->
        </x>
    </x>
</x>
```

Scales attempts to provide validation of the names via the version, but this is more than a little edge case filled.  Other libraries (XOM for example) have chosen to not support XML 1.1s use at all.

See [XML Version Support](XmlVersionSupport.md) for more details.

## Namespaces in Scales

Namespaces are directly modelled as a type in Scales and are used to create QNames.  Namespaces are created simply by

```scala
  // a Namespace
  val ns = Namespace("test:uri")
  // a PrefixedNamespace
  val p = ns.prefixed("p")

  // this QName is fully qualified
  val qname = p("localname")
```

PrefixedNamespaces are required to create a PrefixedQName but can also lead to simpler looking code.  In addition, they can be used to declare namespace mappings on a particular level of a tree (attached to Elem - which can reduce re-use), the XML 1.1 example above requires this approach of declaring the mappings.

## QNames in Scales - Let the compiler help us

Scales enforces the use of QNames throughout its api.  Attributes can '''only''' be created with either a NoNamespaceQName or a PrefixedQName.  Elements also can use UnprefixedQNames:

```cscala
    val ns = Namespace("uri:namespace")
    val pre = ns.prefixed("pre")
    
    val unprefixedQName = ns("localName")
    val prefixedQName = pre("localName")
    // l is a NoNamespaceQName wrapper
    val nonamespaceQName = "localName"l

    val uelem = Elem(unprefixedQName)
    
    // implicitly converted, nonamespaceQName is valid for an attribute
    val nonamespaceAQN : AttributeQName = 
      nonamespaceQName

    // won't compile as unprefixed but namespaced QNames are not valid for an attribute
    //val unprefixedAQN  : AttributeQName = 
    //  unprefixedQName

    val root = 
      <(uelem) /@(nonamespaceAQN -> "nv",
		  prefixedQName -> "pv") /(
	prefixedQName, // these implicitly create an XmlTree with this QName
	nonamespaceQName 
      )
```

## Runtime Validation

Whilst the compiler can help us with correctness on types, we sacrifice a usable api if we force other content checks into the type system.  For example we could model QName.apply functions to return an Either (or Validation) and force combination of all Elem related data, but this would distract from the api (especially considering that incorrect XML is only likely to come from the developer - the parser won't let it through otherwise).

Scales QNames and Namespaces check for correct local name and prefix content at runtime based on the compile time scoped XmlVersion.  This still implies no object will ever get created with incorrect data, but forces these checks into throwing exceptions (or make the user suffer with the api that follows).

Valid characters for a given xml version are checked by Xerces XML Char utilities with the addition of a simple ":" check from Scales.  Developers may not create a Namespace with "" unless using Xml11.  Declaring PrefixedNamespaces with prefix "xmlns" or "xml" must match their predefined uris.

__NB: I've not given up on attempting this completely__

## Equality

Namespaces are equal only when their uri is equal, but PrefixedNamespaces also take the prefix into account.

QNames use both namespace + local name to test for equality:

```scala
  // using the above definitions 
  noNamespace == unprefixed // false

  val unprefixed2 = "fred:uri" :: "local"

  unprefixed2 == unprefixed // false

  // =:= is a seperate method that acts as == but is typed on QNames
  prefixed =:= prefixed2 // true

  val prefixedl2 = pre("local2")
  
  prefixed =:= prefixedl2 // false

  prefixedDirect == prefixed // true
```

For PrefixedQNames its also possible to take the prefix itself into consideration (although not recommended):

```scala
  prefixedDirect =:= prefixed // true

  prefixedDirect === prefixed // false
```

### Scalaz Equal and Scales Equiv

A Scalaz Equal typeclass is defined allowing (but shadowed by QName for its default):

```scala
  import scalaz._
  import Scalaz._

  implicitly[Equal[QName]].equal(prefixedDirect, prefixed) // true
```

Equiv is a helper class from Derek Williams that allows defining conversions to types that have a provided Scalaz Equal type class instance.  This is used by ListSet to allow removal of Attributes by their QName (via =:=).

```scala
  import scalaz._
  import Scalaz._

  // the below is provided by the scales.utils.equivalent
  implicitly[Equiv[QName]].apply(prefixedDirect, prefixed) // true

  equivalent( prefixedDirect, prefixed ) // true

  // PrefixedQName and NoNamespaceQName can be "converted" to QName
  equivalent( prefixedDirect, localOnly) // false
```

## Testing For QNames

Equality and Extractors can cover most usage scenarios but sometimes you want to test and extract in one go - just like Scala's regex support.

As part of the Xml DSL Scales provides both QName and Namespace Matchers.  Starting with the following definitions we'll look at how to use these Matchers:

```scala
  val ns = Namespace("uri:namespace")
  val name = ns.prefixed("pre","localName")

  val elem = Elem(name)
  val attrib = Attribute(name, "val")
```

QNameMatcher allows you to test for whether an Attribute or an Elem is defined by a given QName (via =:=, namespace and local name only are compared):

```scala
  val NamedMatcher = name.matcher // .m is a short cut

  elem match {
    case NamedMatcher(e) => println("Matched element " + asString(e))
    case _ => error("oops")
  }

  attrib match {
    case NamedMatcher(a) => println("Matched attribute " + a)
    case _ => error("oops")
  }
```

NamespaceMatcher simply checks for the Attribute or an Elem being part of a Namespace:

```scala
  val NamespaceMatcher = ns.m // short for matcher
  
  elem match {
    case NamespaceMatcher(e) => println("Matched element " + asString(e))
    case _ => error("oops")
  }

  attrib match {
    case NamespaceMatcher(a) => println("Matched attribute " + a)
    case _ => error("oops")
  }
```

## Serializing QNames

QNames for elements and attributes are considered markup, and depending on both the encoding and Xml Version will throw an InvalidCharacterInMarkup exception with the relevant QName part that caused the issue.