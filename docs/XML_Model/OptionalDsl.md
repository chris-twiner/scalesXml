# Optional Xml DSL

The [XmlDsl.html Xml DSL] provides a simple way to create XML trees via a flexible builder approach.  The Optional Xml DSL provides a similar approach to fully optionl trees, where no empty nodes should be present.  

Given the following xml:

```xml
  <root xmlns="uri:test">
    <optional>some possible text</optional>
  </root>
```

we may like to remove optional if the "some possible text" was not defined (for example against a minoccurs 0 element).

The Optional DSL fully integrates within the Xml DSL itself, allowing easily defined optional subtrees, but uses a different notation to let you know at a glance you are dealing with the Optional DSL:

```scala
  val ns = Namespace("uri:test") 

  val root = ns("root")
  val optional = ns("optional")

  def someOptionalText: Option[String] = ???

  // optional is converted into an instance of OptionalDslBuilder allowing ?~> to be called
  val optionalxml = root /( optional ?~> someOptionalText )
```

If someOptionalText returns Some("a value") optionalxml will serialize to

```xml
  <?xml version="1.0" encoding="UTF-8"?><root xmlns="uri:test"><optional>a value</optional></root>
```

however returning None will collapse the optional element as well as the text (this is in contrast to ~> Option[String] in the normal Xml DSL which leaves the optional element present):

```xml
  <?xml version="1.0" encoding="UTF-8"?><root xmlns="uri:test"/>
```

The api for the Optional DSL can be found [here](../../scales-xml/site/scaladocs/scales/xml/dsl/OptionalDslBuilder.html).

## Cascading Optionals 

The Optional DSL cascades so the following xml:

```scala
  val deepNones = 
    ?<("Alocal"l).?/( 
      ?<("another"l) ?/( 
        ("lowerstill"l) ?~> None ),
      ?<("yetan"l) ?~> None
    ).addNonEmpty(("ShouldNotGetAdded"l))

  val result = deepNones.toOptionalTree
```

will have result.isEmpty == true.

Serializing OptionalDsls doesn't make much sense without at least one outer wrapping node, as such no SerializeableXml type class instance is provided.

The addNonEmpty method above allows adding child nodes directly, but filters out any empty trees (no child nodes or attributes).  It does not, however, perform deep cascading, so prefer OptionalDslBuilder instances where possible.

The "?<" constructor is the Optional DSL counterpart to the Xml DSLs "<" and acts a visual marker.