package scales.xml

import javax.xml.transform.Source
import javax.xml.transform.stax._
import javax.xml.transform.sax._

import trax._

import scales.utils._

import ScalesXml._

/**
 * A TrAX Result
 */ 
case class ScalesResult[Token <: OptimisationToken](strategy : PathOptimisationStrategy[Token] = defaultPathOptimisation) extends SAXResult(new scales.xml.Handler[Token](strategy)) {
  def doc = {
    val handler = getHandler.asInstanceOf[scales.xml.Handler[Token]]
    //val tp = handler.buf.proxies(0)

    Doc(handler.getBuf.tree//Tree(tp.elem, tp.children.result)//path.tree()
	, handler.getProlog, handler.getEnd)
  }
}

/**
 * Use as a basis for TrAX or javax.xml.validation and some other services.  Unfortunately XPath isn't one of them, fortunately ScalesXml comes with similar...
 *
 * To add to the joy, at time of writing Xalan (2.7.1) uses SAXSource.toInputSource to convert this into a stream, problem is the JVM version (1.6.0_24) doesn't support StAXSource transformation, which is nice.  So until this is fixed users should serialize themselves into a stream and then use for trax if they are using Xalan directly.
 * 
 * Only the prolog and endMisc are taken from the document, the rest comes from the stream
 */ 
case class ScalesSource(stream : Iterator[PullType], documentLike : DocLike = EmptyDoc()) extends StAXSource (
  new trax.ScalesStreamReader() {
    val itr = stream
    val docLike = documentLike
  }
)

trait TraxConversionImplicits {

  implicit def treeToSource( tree : XmlTree )(implicit sf : SerializeableXml[XmlTree]) : Source = 
    streamOr( tree, ScalesSource(convertToStream(tree)))(sf)

  implicit def docToSource( doc : Doc )(implicit sf : SerializeableXml[Doc]) : Source = 
    streamOr( doc, ScalesSource(convertToStream(doc.rootElem), doc))(sf)
}


trait TraxSourceConversions {

  def convertToStream(tree : XmlTree) : Iterator[PullType] = new trax.TreeIterable(tree)

  /**
   * When the user really wants a stream source or there is yet another place that jaxp Source support is not complete.
   */ 
  def asStreamSource[T : SerializeableXml]( xml : T ) : Source = {
    val str = asString(xml)
    new javax.xml.transform.stream.StreamSource(new java.io.StringReader(str))
  }

  def streamOr[T : SerializeableXml]( xml : T , f : => Source) : Source =
    if (Versions.traxSourceShouldSerialize)
      asStreamSource(xml)
    else 
      f
  
}

