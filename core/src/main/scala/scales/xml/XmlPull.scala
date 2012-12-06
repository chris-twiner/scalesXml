
package scales.xml

import javax.xml.stream._
import scales.utils.io.{ProxiedCloseOnNeedReader, ProxiedCloseOnNeedInputStream}
import scales.utils.resources.{CloseOnNeed, IsClosed, Pool}

import scales.xml.parser.strategies.{MemoryOptimisationStrategy, PathOptimisationStrategy, OptimisationToken}

import java.io._

/**
 * We should give the stax what it needs to work best, especially with encoding issues, converting streams to readers etc is teh suxor
 */
sealed trait SourceUser extends CloseOnNeed {
  def getReader( xf : XMLInputFactory ) : XMLStreamReader
}

case class CharacterSourceUser(reader : ProxiedCloseOnNeedReader) extends SourceUser {
  def getReader( xf : XMLInputFactory ) = xf.createXMLStreamReader(reader)
  protected def doClose = reader.closeResource
}

case class ByteSourceUser(stream : ProxiedCloseOnNeedInputStream) extends SourceUser {
  def getReader( xf : XMLInputFactory ) = xf.createXMLStreamReader(stream)
  protected def doClose = stream.closeResource
}

/**
 * Wraps the stax cursor inteface (iterator just adds weight here).
 *
 * scala.xml.pull uses a thread based on stax approach to push/pull events.
 *
 * This code uses stax only, extra iteratee goodness will appear curtousy of scalaz....
 *
 */
trait XmlPulls {

  /**
   * Provides proxied sources to the pull parsers, the stream provided is the stream used
   */ 
  def sourceUser( source : org.xml.sax.InputSource ) = {
    // use character stream in preference
    val cs = source.getCharacterStream()
    if (cs eq null) 
      ByteSourceUser(ProxiedCloseOnNeedInputStream(source.getByteStream()))
    else 
      CharacterSourceUser(ProxiedCloseOnNeedReader(cs))
  }


  /**
   * Load xml via pull parsing
   */ 
  def pullXmlCompletely[RToken <: OptimisationToken]( source : org.xml.sax.InputSource, strategy : PathOptimisationStrategy[RToken] = defaultPathOptimisation, parserFactoryPool : Pool[XMLInputFactory] = impl.DefaultStaxInputFactoryPool, closeAfterUse : Boolean = true) : Doc = {
    val pull = pullXml[RToken](source, strategy, parserFactoryPool, closeAfterUse)
    
    Doc(toTree(pull, strategy), pull.prolog, pull.end)
  }

  /**
   * Attempts to convert a stream to a tree
   */ 
  def toTree[RToken <: OptimisationToken]( pull : Iterator[PullType], strategy : PathOptimisationStrategy[RToken] = defaultPathOptimisation ) = {
    val token = strategy.createToken(Xml10, IsFromParser)
    // start with nothing
    val buf = new impl.TreeProxies()

    while( pull.hasNext ){
      pull.next match {
	case Left( i : XmlItem ) => 
	  buf.addChild(i)
	case Left( e : Elem ) => 
	  strategy.beginSubTree(buf, e, token)
	case Right(endElem) => 
	  strategy.elementEnd(buf, token)
      }
    }
    buf.tree
  }

  /**
   * Creates a new XmlPull based on source for direct handling of the stream.  Note to close the stream you must bracket.
   * The individual XmlPull will be closed after the document end but the stream will remain open
   */
  def pullXmlResource[RToken <: OptimisationToken](source: org.xml.sax.InputSource, optimisationStrategy : MemoryOptimisationStrategy[RToken] = defaultOptimisation, parserFactoryPool: Pool[XMLInputFactory] = impl.DefaultStaxInputFactoryPool) : (CloseOnNeed, XmlPull) = {
    val stream = sourceUser(source)

    val pf = parserFactoryPool.grab
    
    implicit val weAreInAParser : FromParser = IsFromParser

    import ScalesXml.defaultVersion

    (stream,
      new XmlPull {
	type Token = RToken
	
	val strategy = optimisationStrategy
	implicit val token = optimisationStrategy.createToken

        val parser = stream.getReader(pf)
        val resourceCloser = () => { parserFactoryPool.giveBack(pf) } //noop its now the CloseOnNeeds job

        start
      })
  }

  /**
   * Creates a new XmlPull based on source.  By default it will close the stream after use.
   */
  def pullXml[RToken <: OptimisationToken](source: org.xml.sax.InputSource, optimisationStrategy : MemoryOptimisationStrategy[RToken] = defaultOptimisation, parserFactoryPool: Pool[XMLInputFactory] = impl.DefaultStaxInputFactoryPool, closeAfterUse: Boolean = true) : XmlPull with java.io.Closeable with IsClosed = {
    val stream = sourceUser(source)

    val pf = parserFactoryPool.grab

    implicit val weAreInAParser : FromParser = IsFromParser
    
    import ScalesXml.defaultVersion

    new XmlPull with java.io.Closeable with IsClosed {
      type Token = RToken

      val strategy = optimisationStrategy
      val token = optimisationStrategy.createToken

      val parser = stream.getReader(pf)
      val resourceCloser = () => { parserFactoryPool.giveBack(pf); stream.closeResource } // bug: 6539065 javadocs say does not close, implementation seems to close :< calling it twice should be safe - especially with CloseOnNeed

      private[this] var closed = false
      def isClosed = closed

      override def internalClose { close }

      def close {
        if (!closed) {
          parser.close
          resourceCloser() // parser close doesn't close the resource
          closed = true
        }
      }

      start
    }
  }

  /**
   * Allows plugging in other feeds, non source based, as such not closeable
   */ 
  def pullXmlReader[RToken <: OptimisationToken]( reader  : XMLStreamReader, defaultOptimisationStrategy : MemoryOptimisationStrategy[RToken] = defaultOptimisation) : XmlPull = new XmlPull {
    type Token = RToken

    implicit val eweAreInAParser : FromParser = IsFromParser

    import ScalesXml.defaultVersion

    val strategy = defaultOptimisationStrategy
    val token = defaultOptimisationStrategy.createToken

    val parser = reader
    val resourceCloser = () => {}

    start
  }

  type PullType = Either[XmlEvent, EndElem]

  implicit def toLeft(ev: XmlEvent) = Left(ev)
  implicit def toRight(ev: EndElem) = Right(ev)

}

/**
 * Exists purely to satisfy staxs events and indicate to the client code that the xml "stack" should be popped
 */
case class EndElem(name: QName, namespaces: Map[String, String] = Map[String, String]())

