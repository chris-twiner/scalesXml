# Async Pull

Traditional push models (DOM/SAX parsing) and the StAX pull standard both block on streams when they need more data.  The [https://github.com/FasterXML/aato-xml Aalto XML] project aims to change that.

Instead of blocking Aalto XML enhances the StAX parser adding an event type EVENT_INCOMPLETE to signal that no more data could be read.  Scales Xml adds the AsyncParser to wrap this behaviour (and a number of supporting io classes) providing a single function to capture the interaction:

```scala
  def nextInput(d: DataChunk): Input[EphemeralStream[PullType]]
```

where the [./doc/scales/utils/io/DataChunk.html DataChunk] ADT is either EOFData, EmptyData or an array of Byte.  The resulting Scalaz IterV.Input provides the mirror of either EOF, Empty or El for a given stream of PullType.

This allows a chunked parsing approach, the developer can fully control the use of the resulting xml.  Calling nextInput with a filled DataChunk won't necessarily return an El if not enough bytes were "pushed" into nextInput.  The resulting EphemeraStream can only be reliably traversed once - and is used as a safer memory usage stream only.

An example direct usage of this api:

```scala
  import scales.utils.io._
  
  var channel: java.nio.channels.ReadableByteChannel = ??? // a channel 
  var wrappedChannel: DataChunker[DataChunk] = channel.wrapped
  var b: DataChunk = EmptyData

  while(b != EOFData) { // real code could return thread to a pool with another thread selecting on multiple channels
    b = wrappedChannel.nextChunk
    val s = parser.nextInput(b)
    s(
      el = e => { // use stream of PullTypes
	var st = e
	while(!st.isEmpty) {
	  val pullType = st.head()
	  // use pullType
	  st = st.tail()
	}
      },
      empty = ,// needs more data
      eof = // xml message is now finished - no more events possible
    )
  }

```

The '''wrapped''' method is an implicit converter that lifts a java.nio channel.ReadableByteChannel into a <nowiki>DataChunker[DataChunk]</nowiki>.  This interface provides nextChunk and a CloseOnNeed with the redundant type parameter allowing an Enumerator to be created over DataChunker (Enumerators can only enumerate over a shape <nowiki>F[_]</nowiki>).

## Integrating With Enumeratees - enumToMany

[ResumableIter'atees](RepeatedSections.md#pull-parsing-resumableiteratees) within Scales allow calculations to be suspended and resumed with intermediate results which is very useful for streamed XML processing.  When a new value is available a Done((value, cont)) is returned and when more data is required a Cont.  The same standard Iteratee semantics (Done or Cont) can be used to "map" over an Iteratee to convert one sequence of typed events into sequences of other types.  This mapping process is modelled by Enumeratees.

The key Enumeratee provided by Scales is [enumToMany](../../scales-xml-{{site_scala_compat()}}/site/scaladocs/index.html#scales.utils.package@enumToMany%5bE,A,R%5d(ResumableIter%5bA,R%5d)(ResumableIter%5bE,EphemeralStream%5bA%5d%5d):ResumableIter%5bE,R%5d), a mapping Enumeratee:

```scala
  def enumToMany[E, A, R]( dest: ResumableIter[A,R])( 
    toMany: ResumableIter[E, EphemeralStream[A]]): ResumableIter[E, R]
```

The interesting part is the EphemeralStream of type A returned by the mapping Iteratee 'toMany', this allows any number of results for a single input of type E.  If toMany or indeed dest returns EOF, so must the resulting Iteratee.

The following simple example shows how enumToMany can work:

```scala
  def iTo(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower > upper) EphemeralStream.empty else EphemeralStream.cons(lower, iTo(lower + 1, upper))

  val i = List(1,2,3,4).iterator

  val (res, cont) = enumToMany(sum[Int])( mapTo( (i: Int) => El(iTo(1, i)) ) )(i).run
  assertEquals(20, res)
  assertTrue("should have been done", isDone(cont))
```

The toMany Iteratee here is mapTo called over the iTo function, for each input i it returns 1 -> i.  The destination Iteratee sums the resulting stream, so the list 1 -> 4 then provides a sequence of 1, 1, 2, 1, 2 3, 1, 2, 3, 4 totalling 20.

In the above example sum and mapTo are simple IterV's that are, in the mapTo case ''restarted'' for each input of "i", and in sum's case is run until EOF is sent.  This restarting with ResumableIter's allows the computation to be continued after intermediate results are returned, making them ideal for XML processing.

## Async Pull with enumToMany

A simple streaming example can be seen below:

```scala
    val url = scales.utils.resource(this, "/data/BaseXmlTest.xml")

    val channel : DataChunker[DataChunk] = Channels.newChannel(url.openStream()).wrapped

    val parser = AsyncParser()

    val strout = new java.io.StringWriter()
    val (closer, iter) = pushXmlIter( strout )

    val enumeratee = enumToMany(iter)(parser.iteratee)
    val ((out, thrown), cont) = enumeratee(channel).run

    assertFalse( "shouldn't have thrown", thrown.isDefined)
    assertTrue("should have been auto closed", closer.isClosed)
    assertTrue("Channel itself should have been auto closed", channel.isClosed)
```

The .iteratee method calls (by default) the AsyncParser.parse function which wraps the AsyncParser into a <nowiki>ResumableIter[DataChunk, EphemeralStream[PullType]]</nowiki>, where the stream itself is the result of calling AsyncParser.nextInput.  It will return Done with a stream of events when enough data is processed.  As can be seen from the type it fits perfectly with toMany parameter of enumToMany, for a given chunk it may return many PullTypes.

The pushXmlIter is a serializing IterV that pushes PullType into an outputstream.  enumToMany then joins these two Iteratees to provide streaming.

The example also shows that the resources are all automatically closed upon completion.  The parser and DataChunker resources themselves are CloseOnNeed instances and therefore also early closing (for example when there has been enough data processed).