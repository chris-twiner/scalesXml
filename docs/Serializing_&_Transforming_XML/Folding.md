# Folding Xml

Scales Xml provides a unique transformation option based on the premise of XmlPaths having document order.  It is therefore possible to navigate between two paths.  If we can do that then we can transform lists of paths within a same document by folding over them.

Once a given path is modified it effectively refers to a new Xml tree, the trick is then to move the zipper to the next paths relative position in the old document.

A number of transformations are available based on [FoldOperation](../../site/scaladocs/scales/utils/collection/path/FoldOperation.html):

* AddAfter - ''add the nodes to the parent after the current path''
* AddBefore - ''add the nodes to the parent before the current path''
* AsIs - ''no-op''
* Remove - ''remove the current node''
* Replace - ''replace the current node''
* ReplaceWith - ''replace the current node with the results of another fold''

AsIs and ReplaceWith deserve explanation before examples.  When performing transformations it is often useful to query or test against the resulting nodes, if the node should not be changed then AsIs() will make the fold a no-op.

ReplaceWith effectively allows nested folds which an important part of the [composing transformations](#composing-transformations).

## PathFoldR - Catchy Result Type

The PathFoldR type is a (for Xml):

```scala
  XmlPath => Either[XmlPath, FoldError] // Either is the FoldR
```

Each foldPosition call is then resulting in either a new XmlPath or a reason as to why the transformation could not complete.  Valid reasons are:

* NoPaths - ''you didn't find any nodes with the path''
* NoSingleRoot - ''if the input nodes don't share a single root path how can we join them''
* RemovedRoot - ''you can't return changed nodes if the root element was deleted''
* AddedBeforeOrAfterRoot - ''you can't add nodes around the root element''

## Examples 

The below examples will use the following base xml and definitions:

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

For a full set examples see the DslBuilderTests.scala.

### Adding Children

The following example will add nodes around the existing nodes:

```scala
  val nodes = top(builder) \* 

  nodes.map(qname(_)) // Child, Child2

  val res = foldPositions( nodes  ){
    case path if (!path.hasPreviousSibling) => AddBefore("start"l)
    case path if (!path.hasNextSibling) => AddAfter("end"l)
    // will throw a MatchError if no _ see AsIs
  }

  asString(res.left.get.tree)
```

Will return the following XML (formatting added for readability):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <start xmlns=""/>
  <Child/>
  Mixed Content
  <Child2>
    <Subchild>text</Subchild>
  </Child2>
  <end xmlns=""/>
</Elem>
```

### AsIs

In the [Adding_Children Adding Children] section we've left a possible match error, for example, if we choose not to act on the last child:

```scala
  // oops, surprising
  val res = foldPositions( nodes  ){
    case path if (!path.hasPreviousSibling) => AddBefore("start"l)
    // will throw a MatchError
  }
```

However, AsIs can be used to make sure we take normal actions if we have no match:

```scala
  val res = foldPositions( nodes  ){
    case path if (!path.hasPreviousSibling) => AddBefore("start"l)
    case _ => AsIs
  }

  asString(res.left.get.tree)
```

Giving:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <start xmlns=""/>
  <Child/>
  Mixed Content
  <Child2>
    <Subchild>text</Subchild>
  </Child2>
</Elem>
```

### Removing Children

In this example we'll remove the SubChild element:

```scala
  val nodes = top(builder) \\* ns("Subchild") 

  nodes.map(qname(_)) // Subchild

  val res = foldPositions( nodes  ){
    _ => Remove()
  }

  asString(res.left.get.tree)
```

Giving:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <start xmlns=""/>
  <Child/>
  Mixed Content
  <Child2/>
</Elem>
```

### Replacing Children

This example changes the text in Subchild:

```scala
  val nodes = top(builder). \\*(ns("Subchild")). \+.text

  nodes.map(string(_)) // Subchild

  val res = foldPositions( nodes  ){
    _ => Replace("another value")
  }

  asString(res.left.get.tree)
```

yields:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <start xmlns=""/>
  <Child/>
  Mixed Content
  <Child2>another value</Child2>
</Elem>
```

## Composing Transformations

Transformations, like the rest of Scales, should also be composable.  It is possible to chain transformations allowing some to fail if they can't find matches - NoPaths - ([| - Try The Next](#-try-the-next)) to find matches or stopping at the earliest failure ([&_-_Fail_Early](#-fail-early)).

In addition, they can be nested, performing transformations within transformations (ReplaceWith).

### ReplaceWith - Nested

ReplaceWith aims to mimic the nesting of matching templates in xslt (via call-template) whereas using the pattern matcher directly more closely resembles apply-templates.

Using this replace as a basis:

```scala
  // for every child element add a text child that contains the qname of the elem
  def addTextNodes( op : XmlPath ) =
    foldPositions( op.\* ) { 
      p => Replace( p.tree / qname(p) ) 
    }

  val allReplaced = addTextNodes( top(builder) )

  asString(allReplaced.left.get.tree)
```

yielding:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <start xmlns=""/>
  <Child>Child</Child>
  Mixed Content
  <Child2>
    <Subchild>text</Subchild>
    Child2
  </Child2>
</Elem>
```

Now we can replace just the Subchild with:

```scala
  val nodes = top(builder). \\*(ns("Child2"))

  val res = foldPositions( nodes  ){
    _ => ReplaceWith(x => addTextNodes(top(x.tree)))
  }

  asString(res.left.get.tree)
```

yielding:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <Child/>
  Mixed Content
  <Child2>
    <Subchild>textSubchild</Subchild>
  </Child2>
</Elem>
```


### & - Fail Early

The "and" chained transformation will stop when it hits any failure.

Using the same base document as before:

```scala
  val wontFindAnyNodes = ( op : XmlPath ) =>
    foldPositions( op \* ns("NotAChild") ) { 
      p => Replace( p.tree / qname(p) ) 
    }

  val willFindANode = ( op : XmlPath ) =>
    foldPositions( op \* ns("Child2") ) { 
      p => Replace( p.tree / qname(p) ) 
    }

  val root = top(builder)

  val combined = wontFindAnyNodes & willFindANode

  val noPaths = combined( root ) // Will be Right(NoPaths)
```

whereas:

```scala
  val alsoFindsANode = ( op : XmlPath ) =>
    foldPositions( op \* ns("Child") ) { 
      p => Replace( p.tree / qname(p) ) 
    }

  val andOk = willFindANode & alsoFindsANode

  val result = andOk( root )

  asString(result.left.get.tree)  
```

yields:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <Child>Child</Child>
  Mixed Content
  <Child2>
    <Subchild>text</Subchild>
    Child2
  </Child2>
</Elem>
```

### | - Try The Next

The "or" chained transformation will try the next transformation if NoPaths is returned.  This allows safe chaining always passing the result through until the first failing transformation.

Using the examples from & above:

```scala
  val orWorks = wontFindAnyNodes | willFindANode

  val orResult = orWorks( root )

  asString(orResult.left.get.tree)  
```

yields:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Elem xmlns="test:uri" xmlns:pre="test:uri:attribs" pre:attr3="val3" attr2="val2" pre:attr1="val1">
  <Child/>
  Mixed Content
  <Child2>
    <Subchild>text</Subchild>
    Child2
  </Child2>
</Elem>
```