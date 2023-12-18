= Pull Parsing =

Scales Pull Parsing leverages StAX, the JAXP streaming api, and Scalaz Iteratees, to allow flexible parsing of large documents or many documents in memory constrained environment (e.g. a high performance server).

Scales, as with full tree parsing, allows configurable optimisation strategies and the pull parser used.  The optimisation strategy type, unlike full tree parsing is a MemoryOptimisationStrategy and does not allow for tree path optimisations.

The input to all pull xml functions is a sax.InputSource, which allows the same conversions as for a full tree parse.

A curio exists with pullXmlCompletely, which uses the pull parser to load xml Docs.  This may be of use in an environment where the StAX parser performs better than SAX, but tests have shown lower memory consumption and higher performance when using SAX to parse full trees.

Some of the advanced features of Pull Parsing may require importing Scalaz as well:

${cscala}
  import scalaz._
  import Scalaz._
  import IterV._ // may not always be required
${cend}

== Pull Model ==

The Scales Pull Model adds only EndElem to create PullType:

${cscala}
  type PullType = Either[XmlEvent, EndElem]
${cend}

Where XmlEvent is exactly the same Elem, XmlItem model as with full Trees.  This is possible because Scales separates the structure of data and the data itself.

The developer only has to learn one single difference to be able to use pull parsing.  For example the code to process a stream is simply:

${cscala}
  val pull = pullXml(source)

  while( pull.hasNext ){
    pull.next match {
      case Left( i : XmlItem ) => 
        // do something with an XmlItem
      case Left( e : Elem ) => 
        // do something with start of a new element
      case Right(endElem) => 
      	// do something with the end of an element
    }
  }
${cend}

== Resource Management ==

The above example has a serious potential flaw, if anything in the while loop throws the resource cannot be closed.  To allow greater control of the resource Scales provides the following interface (full api details present [./doc/scales/utils/resources/CloseOnNeed.html here]):

${cscala}
  trait CloseOnNeed {
    def ++ (close2: CloseOnNeed): CloseOnNeed
    def closeResource : Unit
  }
${cend}

Importantly closeResource only closes a resource once.  This resource is directly available when calling pullXmlResource.

pullXml itself is also a Closable and provides the same guarantee, close only attempts to close the resource once.

Both results provide the isClosed function (via the IsClosed interface) allowing code to trust that it has been closed. (NB - a future version may choose to expose this in the type system, but integrating with the ARM library makes more sense).

What CloseOnNeed also adds is the ++ function, which combines one CloseOnNeed with another to create a new CloseOnNeed that closes the other two resources.  This allows chaining of xml files (via pull iterators).

== Simple Reading Of Repeated Sections ==

Given an xml document with the following format:

${cxml}
  <root>
    <nested>
      <ofInterest> <!-- Collect all of these -->
        <lotsOfInterestingSubTree>
        </lotsOfInterestingSubTree>
      </ofInterest>
..
    </nested>
....
  </root>
${cend}

where the interesting parts are always repeating in the same location, we can model the interesting parts a simple List of QNames (very simplified XPath):

${cscala}
  val pull = pullXml(new java.io.InputStream(""))

  val qnames = List("root"l, "nested"l, "ofInterest"l)

  // only returns /root/nested/ofInterest paths
  val itr : Iterator[XmlPath] = iterate(qnames, pull)
${cend}

The resulting Iterator contains paths with single child parents up to the root and all of the subtree of interest.

For more complex repeated sections see [RepeatedSections.html#Examples here for examples].

== Buffering And Identifying Xml Messages ==

When parsing xml messages it is often necessary to identify the type of the message before further processing, for example what kind of soap request is being sent, or what is the root element?

To help with this issue Scales pull parsing offers the ability to "peek" into an event stream and replay the events again to fully process them.

A simple example is processing soap messages based on the first body element, you may want to choose different code paths based on this, but require elements in the header to do so.  The usage is simple via the [./doc/index.html#scales.utils.package@capture%5bA%5d(Iterator%5bA%5d):CapturedIterator%5bA%5d capture function] and the [./doc/index.html#scales.xml.package@skip(⇒List%5bInt%5d):IterV%5bPullType,PeekMatch%5d skip/skipv functions]:

${cscala}

  val xmlpull = // stream capture

  val captured = capture(xmlpull)
  
  // either the path or None if its EOF or no longer possible
  val identified = skip(List(2, 1))(captured) run

  val processor = identified.map(........

  // restart the stream from scratch
  processor.process(captured.restart)
${cend}

The result from skip is simply <nowiki>Option[XmlPath]</nowiki>, if the stream runs out or its no longer possible to get that position it is None. Only as much of the stream is read as needed, it will stop on the Left(Elem) event.

NB to only identify the first element, simply use skip(Nil) instead (or skipv()).