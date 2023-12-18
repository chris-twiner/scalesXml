# Serialising - Nitty Gritty

While [SerializingIntro.html Serialising] provides a high-level overview of serializing in Scales, this section gets into the specifics of how its implemented and compatibility issues.

Scales attempts the following:

1. Work around a number of bugs and behavioural differences with the various LSSerializer implementations
2. Stop as soon as possible with usable error messages - and give the user the tools to deal with that situation
3. Correctness checking with both XmlVersion and CData handling along with encoding and markup generation
4. Only perform serialisation encoding checks once
5. Correctly re-use Charset encodings
6. Allow plugable Serializers and sensible base implementations - if you don't like the way its working you can enhance it

## Encoding

### XML Names 

When serializing XML there are two traditional problems with inter-operation between creator and reader:

* Characterset of stream
* Encoding of document

The xml parsers themselves *should* handle the former (i.e. via BOM and declarations etc), but the latter has more to do with the writing of the document itself.

XML 1.0 allows the use of a large enough range of unicode names that umlauts are valid but the encoding might not be:

```xml
  <?xml version="1.0" encoding="US-ASCII"?>
  <anrüchig/>
```

This document is itself not valid, as the ü in the markup cannot be represented by US-ASCII.

The default [./doc/scales/xml/serializers/LSSerializerFactory$.html LSSerializerSerializerFactory] provides an implementation of the encF function which abstracts away this issue.  For a given character set it attempts to encode the string replying an <nowiki>Option[Throwable]</nowiki>, with None indicating that it can be serialized with that encoding.  Each serialized QName has its parts checked against the shared global cache, to reduce costs in creating encoders and their use.

A LSSerializerNoCacheFactory can be used if the caching behaviour is not desirable.

### Text Data

All normal character data serialized by the LSSerializer instances will be safe across encodings, as per the spec they will be escaped as character references.  Which actual escaping used (hex or plain numeric) depends on the underlying JAXP implementation.

### Other Markup Character Data

CData, Comments and PI cannot have character references within their content.  As such they behave similarly to the encoding of XML Names, with respective exceptions of CDataCannotBeEncoded, CommentCannotBeEncoded and PICannotBeEncoded.

CData deserves a special mention as many libraries (including the LS spec that Scales leverages) add splitting of cdata to allow extra character encodings.  In part due to the incompatible/incomplete jaxp implementation approaches Scales takes a simple design decision - either the CData can be serialised or not.

Developers are free to customise this approach or indeed translate CData to Text wherever recipient systems will accept it.

## Creating a SerializerFactory

It is advised to use the default factory wherever possible, however the LSSerializerBase and LSSerializer traits provides a useful extension point.  The LSSerializerFactoryXHTML shows an example of how it can be extended.

The interface for SerializerFactory defines a single function:

```scala
trait SerializerFactory {
  def apply[R](thunk: Serializer => R)(data: SerializerData): R
}
```

The serialisation itself is performed in the context of thunk, where thunk accepts a Serializer.  The serializer itself contains a very small interface, with only one quirk:

```scala
  path: List[QName]
```

this allows even the Serializer to make assumptions about the structure based on its XPath (the same structure to the pull parsing onQName function).  In the case of element starting (emptyElement/startElement) the parameters are calculated by the serialize function itself.  This means that whilst the Serializer instance is free to choose how to serializer it is freed from making decisions on what attributes or namespaces or default namespaces are correctly defined for that scope.

Overriding startElement or emptyElement would be the ideal place for applications requiring attributes be ordered.