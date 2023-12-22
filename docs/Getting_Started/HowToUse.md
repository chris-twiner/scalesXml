# How To Use

To use ScalesXml you must use the following imports (where the objects ScalesUtils and ScalesXml import implicits).

```scala
  import scales.utils._
  import ScalesUtils._
  import scales.xml._
  import ScalesXml._

  val prefixedNamespace = Namespace("uri:test").prefixed("pre")
  val prefixedQName = prefixedNamespace("prefixed")

  val elem = Elem(prefixedQName)

  println("local name is "+localName(elem))
```

## Parsing and XPaths

```scala

  // Contains the document
  val doc = loadXml(new FileReader("document.xml"))

  // gets Path from the documents root 
  val path = top(doc)

  // query for all nodes that match the XPath
  path.\*("NoNamespace").\*(prefixedQName)

```
