# Xml DSL and Trees

XML DOMs traditionally create trees by each element "owning" its children.  Scales eschews this model, instead using trees to model the containment, allowing re-use for all of the Scales model.

To simplify both creating and basic manipulation of trees Scales provides a DSL that closely resembles the structure of XML.  The point of focus in the tree (or the depth) is controlled by nesting of the arguments.  The implementing class is [./doc/scales/xml/dsl/DslBuilder.html DslBuilder]

The following example is a quick introduction into how to create trees (also used within the [XPath guide](../Accessing_and_Querying_Data/XPaths.md)):

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

## Tour of the DSL

The tour will use the following definitions:

```scala   
  val ns = Namespace("uri:test") 

  val elem = ns("Elem")
  val child = ns("Child")
  val childl = "Child"l
  val root = ns("Root")
  val child1 = ns("Child1")
  val child2 = ns("Child2")
  val child3 = ns("Child3")
  val fred = ns("fred")
```

## Creating a Tree

To start a tree simply use a qname or elem followed by a DSL operator:

```scala
  val dsl = elem / child

  // <Elem xmlns="uri:test"><Child/></Elem>
  asString(dsl) 
```

or for visual distinction use the <( ) function

```scala
  val dsl2 = <(elem) / child
```

## Adding To The Tree

To add a subelement use:

```scala
  // <Elem xmlns="uri:test"><Child/><Child2/><Child3/></Elem>
  val dsl3 = dsl2 /( child2, child3)
```

The tree can be freely nested and, instead of a sequence of subtrees, a by-name version with taking an Iterable allows you to call other functions to provide the sub-trees. 

## Adding an Attribute

```scala
  // <Elem xmlns="uri:test" attr="fred"><Child/><Child2/><Child3/></Elem>
  val dsl4 = dsl3 /@( "attr" -> "fred" )
```

## Setting Text

As we often set a single string for a given subtree the DSL provides a helpful feature to replace all child nodes with a single text node.

```scala
  // <Elem xmlns="uri:test" attr="fred">a string</Elem>
  val dsl5 = dsl4 ~> "a string"
```

This can, of course, be nested:

```scala
  // res14: String = <Elem xmlns="uri:test" attr="fred"><Child/>
  // <Child2/><Child3/><fred>fred's text</fred></Elem>
  val dsl6 = dsl4 /( fred ~> "fred's text" )
```

## Removing Children

Any child whose QName matches (namespace and localname =:=) will be removed via:

```scala
  // <Elem xmlns="uri:test" attr="fred"><Child2/><Child3/><fred>fred's text</fred><Child xmlns=""/></Elem>
  val dsl7 = (dsl6 -/ child) / childl
```

Note that due to infix limitations of Scala that the following won't compile:

```scala
  // won't compile as its an even number of terms
  val dsl7 = dsl6 -/ child / childl
```

If in doubt use brackets or dot accessors to be specific.

## Removing Attributes

Similar to removing Elems QName and a - do the job:

```scala
  // <Elem xmlns="uri:test"><Child2/><Child3/><fred>fred's text</fred><Child xmlns=""/></Elem>
  val dsl8 = dsl7 -/@ "attr"
```

## Optional XML

When using the DSL to template XML its often necessary to model optional content.  This can be expressed via direct use of Option for an attribute, child or text directly:

```scala
  def template(optionalText : Option[String) =
    ns("Elem") ~> optionalText

  val hasTextChild = template(Some("text"))
  val hasNoChildren = template(None)
```

As hasNoChildren demonstrates, when using None, there will be no child added.  Using "" instead, would add an empty Child, whilst semantically identical upon serialization it changes the meaning of the in-memory DOM itself.

The Attribute and child variants function in the same way, but for some usages a fully cascading solution may be required.  If the elem itself should also not be added when there are no children present then the [OptionalDsl.html Optional DSL] should be used.

## Folding Within The DSL

The XPath fold facility is also present directly from the DSL, letting you stay within the tree building and manipulate at the same time with the full power of the DSL.

The DSL provides three fold functions, fold - returning an Either (as per the normal fold but wrapped with DslBuilder), fold_! (throws upon an error) and fold_?.

The fold_? function is perhaps the most commonly useful, and returns "this" if no folds took place (NoPaths), but throws if an error was found.

In each case the parameters are XmlPath => XPath (to allow selection) and the folder (what should we do with the selection) and is used thusly:

```scala
  // remove all ChildX elements regardless of namespace yielding:
  // <Elem xmlns="uri:test"><fred>fred's text</fred></Elem>
  val dsl9 = dsl8.fold_?( _.\*( x => localName(x).startsWith("Child"))) {
    p => Remove()
  }
```

See [XPath Folds](../Serializing_&_Transforming_XML/Folding.md) for more information and how to use it for transformations.