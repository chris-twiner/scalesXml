package scales.xml.parser.pull

import javax.xml.stream._

import scales.utils._
import scales.xml.parser._
import strategies.{MemoryOptimisationStrategy, OptimisationToken}
import scales.xml.impl.{FromParser, IsFromParser}
import scales.xml.{AttributeQName, Attributes, CData, Comment, DTD, Declaration, DocLike, EndElem, EndMisc, Misc, PI, Prolog, PullType, QName, ScalesXml, Text, Xml10, Xml11}
import scala.annotation.tailrec

/**
  * Basis for xmlpulls, an Iterator[PullType]
  */
trait XmlPull extends Iterator[PullType] with DocLike {

  type Token <: OptimisationToken

  implicit val weAreInAParser : FromParser = IsFromParser

  import ScalesXml.defaultVersion

  protected[xml] val parser: XMLStreamReader
  protected[xml] val resourceCloser: () => Unit

  protected[xml] def internalClose {}

  /**
   * Why is this on a pull parser?  Simple answer is by default it costs little,
   * and by selection the user can optimise whole streams at an early and central place.  Optimising the stream in onQNames or another iteratee or in user code seems to go against the definition of a good design.
   */
  protected[xml] val strategy : MemoryOptimisationStrategy[Token]
  protected[xml] val token : Token

  protected[xml] val iStrictPath: List[QName] = Nil
  private[xml] var path: List[QName] = Nil

  private[xml] var current: PullType = null

  /*
     * DocLike parts follow
     */

  private[xml] var vprolog: Prolog = Prolog()
  private[xml] var emisc: EndMisc = EndMisc()

  def prolog = vprolog
  def end = emisc

  /**
   * If the depth is -1 then we haven't hit an element yet, and
   * start should keep pumping until that happens.
   *
   * If the depth is -1 after processing the end root (haveRoot), then we keep pumping into the endMisc
   */
  protected[xml] var depth = -1
  protected[xml] var haveRoot = false

  /**
   * Pumps until the first elem, always collecting the prolog
   */
  protected[xml] def start = {
    while (depth == -1) {
      current = pumpEvent
      if (current.isLeft && (current.left.get eq PullUtils.dtdDummy)) {
        vprolog = vprolog.copy(dtd = Some(
          DTD("", "", "") // DTD has funnyness TODO find out what it looks like
          ))
      }

      if (depth == -1) {
        vprolog = vprolog.copy(misc = vprolog.misc :+ PullUtils.getMisc(current, "prolog"))
      }
    }
  }

  final val it = this: Iterator[PullType]

  def hasNext = current ne null

  def next: PullType = {
    val c = current // cache current
    if (current eq null) throw new NoSuchElementException("The end of the document has been reached")

    current = pumpEvent // pump for the next
    if ((current ne null) && current.isRight && depth == -1) {
      // we are now into the end doc, no more events will be pumped
      var ends = pumpEvent
      while (ends ne null) {
        emisc = emisc.copy(misc = emisc.misc :+ PullUtils.getMisc(ends, "document end Misc"))
        ends = pumpEvent
      }
    }

    c // return cached
  }

  protected[xml] def pumpEvent: PullType = {
    if (!parser.hasNext) return null

    var nextEvent = XMLStreamConstants.END_DOCUMENT
    try {
      val (event, num, odepth, oprolog, opath) = PullUtils.pumpEvent(parser, strategy, token, vprolog, depth, iStrictPath, path) { _ => pumpEvent }
      nextEvent = num
      depth = odepth
      vprolog = oprolog
      path = opath
      event
    } finally {
      // should we close it?
      if (nextEvent == XMLStreamConstants.END_DOCUMENT) {
        internalClose
      }
    }
  }
}

object PullUtils {
  private[xml] final val StartDepth = -1

  implicit val weAreInAParser : FromParser = IsFromParser

  private[scales] val dtdDummy = PI("onlyforme", "init")

  def getMisc(c: PullType, in: String): Misc =
    c.fold[Misc](e => e match {
      case ev: Comment => Left(ev)
      case ev: PI => Right(ev)
      case _ => error("Got an event (" + e + ") that should not be in the " + in)
    }, f => error("End element found in " + in + " " + c))
  // it must be a left and a comment or pi

  def getAttributes[Token <: OptimisationToken]( parser: XMLStreamReader, strategy : MemoryOptimisationStrategy[Token], token : Token ): Attributes = {
    import ScalesXml.toQName

    val count = parser.getAttributeCount()

    val ar = strategy.attributeArray(count, token)
    var i = 0
    //var map = emptyAttributes
    while (i < count) {
      val jqname = parser.getAttributeName(i)
      val pre = jqname.getPrefix
      val local = jqname.getLocalPart
      // attr qnames must be either prefixed or no namespace
      val aqname: AttributeQName =
        if ((pre eq null) || (pre.length == 0))
          // no namespace
          strategy.noNamespaceQName(local, token) // Right)
        else
          strategy.prefixedQName(local, jqname.getNamespaceURI, pre, token) // Left )

      //map = map unsafePlus
      ar.update(i,
	strategy.attribute(aqname,
		       parser.getAttributeValue(i), token)
		)
      i += 1
    }
    scales.xml.impl.AttributeSet.unsafe(ar, count)
  }

  def getNamespaces[Token <: OptimisationToken]( parser: XMLStreamReader, strategy : MemoryOptimisationStrategy[Token], token : Token ): Map[String, String] = {
    val count = parser.getNamespaceCount()
    var i = 0
    var map = Map[String, String]()
    while (i < count) {
      val pre = parser.getNamespacePrefix(i)
      if (pre ne null) {
        map += (pre -> parser.getNamespaceURI(i))
      } // else nothing, the element will define it, question is should we accept it here as it was defined as such!!??
      i += 1
    }
    map
  }

  def getElemQName[Token <: OptimisationToken]( parser: XMLStreamReader, strategy : MemoryOptimisationStrategy[Token], token : Token ) = {
    // elems can have all three, prefixed, ns and none
/*    val jqname = parser.getName()
    val ns = jqname.getNamespaceURI
    val pre = jqname.getPrefix
    val local = jqname.getLocalPart
*/
    val ns = parser.getNamespaceURI
    val pre = parser.getPrefix
    val local = parser.getLocalName

    if ((pre eq null) || (pre.length == 0)) {
	// ns only or none
      if ((ns eq null) || (ns.length == 0))
        strategy.noNamespaceQName(local, token)
      else
	strategy.unprefixedQName(local, ns, token)
    } else
      strategy.prefixedQName(local, ns, pre, token)

  }

  def pumpEvent[Token <: OptimisationToken](parser: XMLStreamReader,
                                            strategy: MemoryOptimisationStrategy[Token],
                                            token: Token,
                                            prolog: Prolog,
                                            idepth: Int,
                                            strictPath: List[QName] = Nil,
                                            ipath: List[QName]= Nil)(otherEventHandler : Int => PullType) : (PullType, Int, Int, Prolog, List[QName]) = {
    var depth = idepth
    var vprolog = prolog
    var vpath = ipath

    @tailrec
    def dropWhile(): PullType = {
      def doStep(event: Int): Option[PullType] = {
        event match {
          case XMLStreamConstants.START_ELEMENT =>
            depth += 1
            val elemQName: QName = PullUtils.getElemQName(parser, strategy, token)
            vpath = vpath :+ elemQName
            if (vpath.equals(strictPath)) {
              val attributes = PullUtils.getAttributes(parser, strategy, token)
              val namespaces = PullUtils.getNamespaces(parser, strategy, token)
              Some(strategy.elem(elemQName, attributes, namespaces, token))
            } else {
              None
            }
          case XMLStreamConstants.END_ELEMENT =>
            depth -= 1
            val isMissingStrictPath = vpath.equals(strictPath.take(strictPath.size - 1))
            if (isMissingStrictPath) {
              val elemQName = PullUtils.getElemQName(parser, strategy, token)
              val namespaces = PullUtils.getNamespaces(parser, strategy, token)
              vpath = ipath.take(ipath.size - 1)
              Some(EndElem(elemQName, namespaces))
            } else {
              vpath = vpath.take(vpath.size - 1)
              None
            }
          case XMLStreamConstants.END_DOCUMENT =>
            Some(null)
          case _ =>
            None
        }
      }

      if (parser.hasNext) {
        doStep(parser.next()) match {
          case Some(res) => res
          case None => dropWhile()
        }
      } else {
        error("this should never happen")
      }
    }

    var nextEvent = XMLStreamConstants.END_DOCUMENT // use this in the case of error from calling next as well, blow it up but try to shut down
    nextEvent = parser.next

    val event: PullType = nextEvent match {
      case XMLStreamConstants.START_ELEMENT => //1
        depth += 1
        if (strictPath.isEmpty)
          strategy.elem(getElemQName(parser, strategy, token), getAttributes(parser, strategy, token), getNamespaces(parser, strategy, token), token)
        else {
          val elemQName = PullUtils.getElemQName(parser, strategy, token)
          vpath = ipath :+ elemQName
          val validSubtree = vpath.take(strictPath.size).equals(strictPath)
          if (idepth == StartDepth || validSubtree) {
            val attributes = PullUtils.getAttributes(parser, strategy, token)
            val namespaces = PullUtils.getNamespaces(parser, strategy, token)
            strategy.elem(elemQName, attributes, namespaces, token)
          } else {
            dropWhile()
          }
        }
      case XMLStreamConstants.END_ELEMENT => //2
        depth -= 1
        if (strictPath.isEmpty)
          EndElem(getElemQName(parser, strategy, token), getNamespaces(parser, strategy, token))
        else {
          val elemQName = PullUtils.getElemQName(parser, strategy, token)
          val namespaces = PullUtils.getNamespaces(parser, strategy, token)
          vpath = ipath.take(ipath.size - 1)
          EndElem(elemQName, namespaces)
        }
      case XMLStreamConstants.CHARACTERS => Text(parser.getText)
      case XMLStreamConstants.CDATA => CData(parser.getText)
      case XMLStreamConstants.COMMENT => Comment(parser.getText)
      case XMLStreamConstants.PROCESSING_INSTRUCTION => PI(parser.getPITarget(), parser.getPIData())
      case XMLStreamConstants.SPACE => Text(parser.getText) // jdk impl never calls but to be safe we should grab it
      case XMLStreamConstants.START_DOCUMENT => {
        // get the encoding etc
	// NB the asynch variety can also call this, if no more events are available then it returns the waiting object.
        val ec = parser.getCharacterEncodingScheme()

        vprolog = vprolog.copy(decl = Declaration(
          version = if (parser.getVersion() == "1.1")
            Xml11 else Xml10,
          encoding = if (ec eq null) defaultCharset else java.nio.charset.Charset.forName(ec), // TODO what do we do about unsupported, throwing is probably fine, but it irritates, if we can get here the parser at least supports it, even if we can't write to it
          standalone = parser.isStandalone()))

        val (nev, nex, nde, vvp, _) = pumpEvent(parser, strategy, token, vprolog, depth)(otherEventHandler) // we don't want to handle this

	// reset to keep the correct values
	nextEvent = nex
	depth = nde
	vprolog = vvp
	nev
      }
      case XMLStreamConstants.DTD => dtdDummy // push it through in start

      // we don't really want to handle other types?
      case _ => otherEventHandler(nextEvent)
    }
    (event, nextEvent, depth, vprolog, vpath)
  }

}
