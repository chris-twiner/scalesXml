= An Introduction to Serializing Xml With Scales =

Scales Xml delegates as much serializing as possible to the DOML3 LSSerializer framework.  There are a number of pain points, due to incompatibilities between jaxp implementations and jvm versions, that Scales smoothes out for the developer.

The main aims with Scales serialisation are: 

* A unified interface
* Correctness ''especially with a view to encoding issues''

The unified interface is one main function:

${cscala}
  def serialize[T: SerializeableXml](pout: XmlOutput)(it: T) : Option[Throwable]
${cend}

All XML types that can be serialized provide a SerializeableXml type class instance and the user supplies the [./doc/scales/xml/serializers/XmlOutput.html XmlOutput] object.  The users main interaction is through the [./doc/scales/xml/serializers/SerializerData.html SerializerData] object allowing users to supply the output java.io.Writer and override both the encoding and xml version used.

Typically one of the following functions will be used:

* [./doc/index.html#scales.xml.package@writeTo%5bT%5d(T,Writer,Option%5bXmlVersion%5d,Option%5bCharset%5d)(SerializerFactory,SerializeableXml%5bT%5d):Option%5bThrowable%5d writeTo( it, out )]
* it [./doc/index.html#scales.xml.WriteTo@writeTo(Writer)(SerializerFactory):Option%5bThrowable%5d writeTo] out
* [./doc/index.html#scales.xml.package@asString%5bT%5d(T)(SerializerFactory,SerializeableXml%5bT%5d):String asString( it )]
* [./doc/index.html#scales.xml.package@itemAsString(XmlItem)(SerializerFactory):String itemAsString( xmlItem )]
* [./doc/index.html#scales.xml.package@printTree%5bT%5d(T)(SerializerFactory,SerializeableXml%5bT%5d):Unit printTree( it )]

The first two (writeTo) are the most general and the later three useful for debugging.

No SerializeableXml type class instances are defined for XmlPaths as they can be either an XmlItem or Tree.  As such itemAsString exists for those times you really want to debug just the XmlItem.

In all cases the xml is written out using the serialize function, which always writes out the xml declaration with the ''pout : XmlOutput'' parameter.  The more general writeTo approach will take the documents declaration unless specifically overridden.

== writeTo & writeTo ==

The writeTo function: 

${cscala}
 def writeTo[T](it: T, output: Writer, 
     version: Option[XmlVersion] = None, encoding: Option[Charset] = None)
     (implicit serializerFI: SerializerFactory, sxml: SerializeableXml[T])
     : Option[Throwable]
${cend}

requires two parameters, the item to be serialized and the output Writer.  When the remaining two parameters are None (or simply left as default) the encoding and xml version are taken from the items document.

To make life easier in the common case the WriteTo class (and implicits from ScalaXml._) allows a simpler:

${cscala}
  val testXml = loadXml(...)
  val str = asString(testXml)

  val out = new java.io.StringWriter()
  testXml writeTo out

  assertEquals(str, out.toString)
${cend}

== What Can Be Serialized? ==

The following types can be serialized:

* XmlTree
* DslBuilder
* Doc
* Elem - an empty elem
* <nowiki>Iterator[PullType]</nowiki> - requires a full stream but works for all flavours of Pull

With the itemAsString allowing simple debug output.

If an object can be meaningfully written as Xml it is suggested to wrap [./doc/index.html#scales.xml.ScalesXml$@streamSerializeable:xml.SerializeableXml%5bIterator%5bxml.PullType%5d%5d streamSerializeable] directly, converting your object into a stream as required. See [./api.sxr/scales/xml/serializers/SerializeableXmls.scala.html here] for code examples (e.g. pullOnlySerializable).