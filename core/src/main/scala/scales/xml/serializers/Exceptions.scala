package scales.xml.serializers

/**
 * Base exception marker for serialization
 */
class SerializationException(val what: String) extends RuntimeException(what) {
  //  override def fillInStackTrace() = this
}

class CannotBeEncoded(override val what: String) extends SerializationException(what)

/**
 * The serialization encoding cannot support the characters in a given markup (attribute name or element name. ächtung in UTF-8 works but not in US-ASCII.
 */
case class InvalidCharacterInMarkup(override val what: String) extends SerializationException(what)

/**
 * The CData content must be split due to encoding issues, which isn't supported (due to bad CData handling on Sun JRE and differences with Xalan.
 *
 * NOTE The Xalan behaviour follows the DOMConfiguration behaviour but will split on encoding issues, which breaks the point of using CData, so if you need that, don't use it, let your sax / pull parser handle proper serilization and escaping for you in a normal text field.
 */
case class CDataCannotBeEncoded(override val what: String) extends CannotBeEncoded(what)

/**
 * Comments don't support & recognition, which means you can't escape the encoding, either they encode or they do not
 */
case class CommentCannotBeEncoded(override val what: String) extends CannotBeEncoded(what)

/**
 * PIs also suffer a bad specification
 */
case class PICannotBeEncoded(override val what: String) extends CannotBeEncoded(what)

/**
 * For a given content it could not be serialized in the document.
 */
case class CannotSerialize(override val what: String) extends SerializationException(what)

/**
 * Result of trying to serialize a 1.1 ncname into a 1.0 document
 */
case class IncompatibleQNameVersions(override val what: String) extends SerializationException(what)
