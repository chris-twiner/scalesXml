package scales.xml

import javax.xml.parsers._
import javax.xml.stream._
import javax.xml.validation._
import javax.xml.transform._

import org.xml.sax.XMLReader
import org.xml.sax.helpers.XMLReaderFactory

/**
 * Most of the JAXP, STAX and DOM apis don't mention much about
 * thread safety but the dominant xerces is completely unsafe with regards
 * to threads.  In fact only the 1.4.2 apis mention anything about thread safety
 * expectations.
 *
 * The process of getting a factory is very expensive and are not thread safe (you can't trust it to create a document in parallel).  See MUSE 270 for an example of why.
 *
 * As such we must use a pool for all handling.  A thread/piece of code
 * grabs a factory from the pool, creating if necessary.
 *
 * To aid the user the parsing code uses the Loaner interface (for SAX and DOM factories) and uses the Pool interface directly for Pull Parsers, as using them does not imply a given scope.
 *
 * By default no validating is performed
 */ 
trait XmlFactories {

}

/**
 * Lazy val needed to trap impl, need pluggable (slf4j style) to swap out different
 * logic, defaulting to sun jaxp ?  For a future version, env property is enough for now.
 *
 */ 
object Versions {
}
