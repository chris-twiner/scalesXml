package scales.xml.trax

import javax.xml.transform.Source
import javax.xml.transform.stax._
import javax.xml.transform.sax._

import scales.utils._

//import ScalesXml._

import scales.xml.parser.sax.Handler
import scales.xml.parser.strategies.{PathOptimisationStrategy, OptimisationToken}

import scales.xml.{Doc, DocLike, EmptyDoc, PullType, XmlTree, streamOr, SerializeableXml, convertToStream, asString, ScalesXml}
import scales.xml.impl.Versions

/**
 * A TrAX Result
 */ 
case class ScalesResult[Token <: OptimisationToken](strategy : PathOptimisationStrategy[Token] = scales.xml.defaultPathOptimisation) extends SAXResult(new Handler[Token](strategy)(ScalesXml.defaultVersion)) {
  def doc = {
    val handler = getHandler.asInstanceOf[Handler[Token]]
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
  new ScalesStreamReader() {
    val itr = stream
    val docLike = documentLike
  }
)

trait TraxConversionImplicits {

  implicit def treeToSource( tree : XmlTree )(implicit serf: scales.xml.serializers.SerializerFactory, sf : SerializeableXml[XmlTree]) : Source = 
    streamOr( tree, ScalesSource(convertToStream(tree)))

  implicit def docToSource( doc : Doc )(implicit serf: scales.xml.serializers.SerializerFactory, sf : SerializeableXml[Doc]) : Source = 
    streamOr( doc, ScalesSource(convertToStream(doc.rootElem), doc))
}


trait TraxSourceConversions {

  def convertToStream(tree : XmlTree) : Iterator[PullType] = new TreeIterable(tree)

  /**
   * When the user really wants a stream source or there is yet another place that jaxp Source support is not complete.
   */ 
  def asStreamSource[T]( xml : T )(implicit serf: scales.xml.serializers.SerializerFactory, serXml: SerializeableXml[T]) : Source = {
    val str = asString(xml)
    new javax.xml.transform.stream.StreamSource(new java.io.StringReader(str))
  }

  def streamOr[T]( xml : T , f : => Source)(implicit serf: scales.xml.serializers.SerializerFactory, serXml: SerializeableXml[T]) : Source =
    if (Versions.traxSourceShouldSerialize)
      asStreamSource(xml)
    else 
      f
  
}

