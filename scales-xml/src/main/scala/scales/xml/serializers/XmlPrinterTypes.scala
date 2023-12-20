package scales.xml.serializers

import scales.utils._

import scales.xml.{DocLike, QName, XmlVersion}

import scala.collection.immutable.Map

import java.io.Writer

import java.nio.charset.Charset

/**
 * Type class for choosing a serializing algo
 */
trait SerializeableXml[T] {
  /**
   * When provided a given T, returns the DocLike object for it
   */ 
  def doc(it: T): DocLike
  
  /**
   * When given a T and the required output and serializer the type class instance will serializers to the XmlOutput using the serializer.
   *
   * Returns the resulting XmlOutput and an exception if an error occurred.
   */ 
  def apply(it: T)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable])
}

/**
 * This class represents state during a serialization
 */
case class XmlOutput(data: SerializerData,
  currentMappings: List[Map[String, String]] = List[Map[String, String]](
    Map[String, String]() + ("" -> "") // default namespace 
    ), path: List[QName] = List())(implicit serializerFI: SerializerFactory) {
  implicit val serializerF = serializerFI
}

/**
 * Wraps the use of writeTo allowing: xml writeTo output
 */ 
case class WriteTo[T : SerializeableXml](it: T, version: Option[XmlVersion] = None, encoding: Option[Charset] = None){

  /**
   * type specific forward to copy, allows overriding of the parameters and converting to WriteTo in one go.
   */ 
  def writeWith(it : T = it, version: Option[XmlVersion] = None, encoding: Option[Charset] = None) = copy(it, version, encoding)
  
  def writeTo(output : Writer)(implicit serializerFI: SerializerFactory) = scales.xml.writeTo(it, output, version, encoding)

}

/**
 * A NamespaceContext represents the prefix->namespace mappings for a given elements children.  It is only valid for the time of serialization (or comparison).
 *
 * @param mappings prefix -> namespace
 * @param declMap prefix -> namespace (these should be declared for a given element)
 * @param addDefault should a new xmlns="" default be added, if so Some(String)
 */ 
case class NamespaceContext( mappings : Map[String, String], declMap : Map[String, String], addDefault : Option[String] )
