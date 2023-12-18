# Scales Xml ${project.version} Overview

Scales Xml, in addition to immutability, aims to focus on these separate concerns:

__Correctness__

* XmlItems such as Text, Comments, PIs and CData are not containing nodes, they cannot have children and they cannot contain invalid characters 
* QNames are a fundamental piece of XML and are treated as first class citizens
* Elements are a QName, the attributes they contain, the namespaces they declare and also do not directly have children
* Attributes are combinations of either a PrefixedQName or NoNamespaceQName and a value

__Structure and manipulation__

* Structure of XML (Tree) is separated from the contents of the Xml, allowing maximal re-use
* Operations on XML (Path - a Zipper over Tree) is separated from both the contents and the structure
* Changing any update to individual XML Element or Items will not cause cascading changes in the rest of the structure (function of Path)
* Transformations on XML Trees (via Path) should be composable and simple to follow.
* The data model is the same for both Push and Pull parsing (with the addition of EndElem for XmlPull)

__XML Comprehensions - XPath__

* Navigation through XML is either through Path directly or the inbuilt XPath syntax
* Syntax closely resembles normal XPath leveraging, for example, Paths ability to navigate parents
* Syntax should separate the navigation of elements from available predicates as far as possible (niceties such as \*QName remain) 
* Syntax should not rely on string interpretation but on types (QName objects used directly)
  * String based XPath 1.0 querying based on Jaxen is however available
* XPaths are used as a basis for Tree transformation
* The full XPath axe are available (with the exception of the namespace axis)

__Pull API__

* Based on javax.xml.stream, allowing optimisations from jaxp/stax upgrades
* Uses the same types as the SAX based parser
* Leverages Scalaz Iteratees to provide processing of the event stream
  * You can choose how to process an xml stream, should your code control the flow, the enumerator or a mixture?
  * Immutable transformations on streams
* Provides an Iteratee to process XPath like locations via a ${cscala}<nowiki>List[QName]</nowiki>${cend}
* Provides a combinator (onDone) to iterate over ResumableIters allowing simplified XML parsing
* Non blocking IO based parsing via Aalto
* Scales adds the concept of resumable iteration:

```scala
  // think resumable folds without cps plugin
  type ResumableIter[E,A] = IterV[E, (A, IterV[E,_])]
```

__Comprehensive Serialisation Support__

* SerializerFactory / Serializer Typeclasses allow plugable serialisation schemes
* SerializeableXml Typeclass allows serializing of all XML through a single consistent interface
  * Lazily create XML using EphemeralStreams and <nowiki>Iterator[PullType]</nowiki>
* Developers can customise the serialisation process while maintaining correctness
  * Use XHTML style tag ends <el /> for empty elements
  * Serialise your attributes in an order of your choosing
  * Or choose to abandon certain correctness checks in favour of speed when you know that your XML inputs and outputs are always correct

__JAXP Support__

* XSLT transformation support
* Conversion to other XML DOMs
* Use of JAXP serialisation - e.g. pretty printing
* javax.xml.validation support
* Aims to hide all JAXP implementation inconsistencies

__Xml Equality__

* Compare xml items against each other
* Full control over QName handling
* Full control over QName values (attributes or text)
* Provides information about what is different and where
* Provides a simple conversion to scalaz.Equal

__Performance__

* Up to 20% faster than scala.xml
* Lower memory usage than both scala.xml and JAXP/DOM
* Parsing memory usage can be optimised for given usage

