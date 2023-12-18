= TrAX & XSLT Support =

JAXP's TrAX API provides both serialisation and transformation to the jvm.  Scales leverages this api to allow xslt transformation and conversions to other DOMs.  Additionally Scales works around known compatibility issues with the Sun JVM and the Xerces implementations (with a nod to the Saxon for its extra support).

Scales will attempt to, when the JVM/JAXP impl supports it, provide a StAXSource enabling transformation without intermediary objects.  This too, unfortunately, doesn't always correctly work across JAXP implementations, some versions ignoring prolog content (some even NPEing when any is present).

As with most Scales JAXP integration the fall-back position of correctly serializing first is always available via the scales.traxSourceShouldSerialize property, when defined and true serialisation will occur before attempting a transformation.  When its not defined a best effort based on the transformer class name is used.

The use of TrAX is generally simple enough not to require further wrapping, and Scales offers Folding for a more fitting transformation approach.

Developers wishing pretty printing can also use TrAX to achieve this (as with a DOM object).

== Simple Usage Example ==

Roundtripping with trax:

${cscala}
  val elem = Elem("trax"l)
  val doc = Doc( elem / elem )

  import javax.xml.transform._
  val trax = TransformerFactory.
    newInstance.newTransformer

  val wr = new java.io.StringWriter()
  val str = new stream.StreamResult(wr)
  trax.transform(doc, str)
  println("source only " + wr.toString)
    
  val sr = ScalesResult()
  trax.transform(doc, sr)
    
   println("roundtrip " + asString(sr.doc))
${cend}

yields:

${cxml}
source only <?xml version="1.0" encoding="UTF-8"?><trax><trax/></trax>
roundtrip <?xml version="1.0" encoding="UTF-16"?><trax><trax/></trax>
${cend}

Note the utf-16 encoding, this was dictated by TrAX, and kept by Scales in the resulting Doc.