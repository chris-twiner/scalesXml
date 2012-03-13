= XmlItem =

Scales represents non Elem data as XmlItem, this includes Text, CData, PI (processing instructions) and Comment.  They share the base trait XmlItem which provides one member:

${cscala}
  val value : String
${cend}

With PI also adding:

${cscala}
  val target : String
${cend}

== Declaring ==

${cscala}
  val text = Text("A text value")
  val cdata = CData("Some cdata")
  val comment = Comment("A comment")
  val pi = PI("Target id","instruction value")
${cend}

== XmlItems Are Reusable ==

XmlItems, as with the rest of Scales - having separated data from structure, have no notion of ownership.

This implies they can be re-used both within and across documents.  This follows with Elems and Trees themselves allowing whole sections of XML to be re-used.

== Runtime Correctness Checks ==

There are a number of simple rules for XmlItems, driven by the spec:

# Comments cannot contain the text "--"
# CData cannot contain "]]>"
# PIs cannot contain "?>" in the value or target
# PIs target when lower cased cannot start with "xml"

== Serializing XmlItems ==

Text, Comments and PI may contain character references and can be correctly serialized regardless of the encoding or Xml Version used.

=== Serializing CData ===

CData is problematic to serialise due to:

* JAXP differences (Xerces/Xalan, Saxon and Sun jdk) all behave differently with regards to CData serialisation
* CData itself can be encoded but not < and &, doing so changes the CData.
* CData, similar to QNames, cannot be encoded via character references

The problems here should warn the user not use CData at all, as per ERH it does not add anything.  It is only provided in Scales to allow documents to be passed through as is (e.g. content based routing).

Scales does its best to work around such issues including, forbidding CData splits, custom serializing (jre will write no end part to cdata) and verifying that the data can be written at all in the documents encoding.