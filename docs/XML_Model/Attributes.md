# Attributes

XML Elements can contain attributes, these are made of a pair: QName and a string value.  Unlike Elements the QName of an Attribute must be either a fully qualified (PrefixedQName) or unqualified name (NoNamespaceQName).

## Defining an Attribute

### Explicitly

```scala

  val ns = Namespace("uri:namespace")
  val pre = ns.prefixed("pre")
   
  val prefixedQName = pre("localName")
  val nonamespaceQName = "localName"l
  val unprefixedQName = ns("localName")

  val nonamespaceAttr = Attribute(nonamespaceQName, "value")
  val prefixedAttr = Attribute(prefixedQName, "value")

  // won't compile as Attributes can't have a namespace without a prefix
  //val unprefixedAttr = Attribute(unprefixedQName, "value")

```

### Implicitly

```scala
  // This can be used when defining elements or within the dsl
  val attr : Attribute = nonamespaceQName -> "value"

  // won't compile
  // val noAttr : Attribute = unprefixedQName -> "value"
```

## Equality

The XML specifications require that no two attributes that share namespace and localName may be in the same element.  Attributes however must be testable for equality outside of this constraint:

```scala
  val attr2 : Attribute = nonamespaceQName -> "another value"

  attr.name == attr2.name // true

  attr == attr2 // false, values are different

  val attr3 : Attribute = nonamespaceQName -> "another value"
  
  attr3 == attr2 // true
```

### Within an Elem

Scalaz Equal comes to the rescue again, we can separate the notion of attribute equality for simple comparisons and those of an Elem's requirements:

```scala
  attr3 === attr // true - we don't take the value into account  

  attr3 == attr // false - values are compared
```

### Attributes ListSet

The Scales Utils class ListSet compares using the notion of Equiv equivalence (see [Equiv and Equal](QNames.md#scalaz-equal-and-scales-equiv) for more details).  To make things easier for developers not using the DSL, Scales adds the Attribs() constructor (which ensures the appropriate Equal and Equiv type classes are always present):

```scala
  val attributes = Attribs(attr, prefixedQName -> "yet another value")
```

## Testing Against QNames or Namespaces

QNameMatcher and Namespace matcher provide simple matching logic that can simplify certain types of pattern matches, just like Scala Regex - see [Testing For QNames](QNames.md#testing-for-qnames) for examples.