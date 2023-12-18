# Scales XML Version Support

There are two versions of XML, 1.0 and 1.1, with 1.0 being most commonly used.

1.1 parsers can parse 1.0 xml, but not the other way around, which adds for another source of potential differences in user experience.

## Differences Between 1.0 and 1.1

There are four main differences:

1. Unicode markup - elements can have names in Kanji
2. Certain whitespace is (dis)allowed
3. Namespaces can be "unbound" from their prefixes
4. Namespaces are IRIs not URIs

These differences make 1.1 support difficult to pleasantly achieve and, in order to keep a uniform interface, forces some correctness checks into the runtime.

## How Does Scales Allow Both Versions ?

Scales takes a pragmatic approach to supporting both versions via using implicits, with the defaultVersion being 1.0.  Override the implicit to provide scoped XML 1.1 support:

```scala
  // scope here is Xml10
  {
    implicit val defaultVersion = Xml11 // scope here is Xml11
    val ns = Namespace("http://www.w3.org")
    val pre = ns.prefixed("n1")
    val disabled = Elem("x"l, Namespace("").prefixed("n1"))
    val a = Elem(pre("a"))
    val x = Elem("x"l, pre)
  }
```

All of the above example namespaces, local names and prefixes are then validated for correctness at runtime.  The above example with Xml10 would throw as "" is not a valid namespace for XML 1.0 (and is only used in XML 1.1 for unbinding namespace prefixes).

### In Parser We Trust - Users We Protect

Runtime checks for correctness are redundant if the model is created via a parser.  The parser already checks the validity (users may choose to override the settings at the factory level).

However, for general usage it's helpful for the library to safeguard us against mistakes.  Scales emphasis on correctness continues here as well.

The implicit FromParser is used by Scales to provide this runtime check, and the QNameCharUtils functions provide the helpers for a given version (via Xerces XMLChar and XML11Char.

## Runtime XmlVersion QName Related Correctness

The implicitly scoped version dictates what runtime checks are applied.  In particular the namespace, local name and prefix are all validated at runtime for their content.  This is true for both Attribute and Elem QNames.

Serializing is also, of course, affected by the XML version.  The document serialisation itself then requires checking the compatibility of QNames.  The rule here is simply if it's an Xml11 QName only (all parts are correct) and the document version is Xml10 its an error.

