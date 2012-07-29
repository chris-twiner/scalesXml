
package scales.xml

import javax.xml.stream._
import scales.utils._

import java.io._

/**
 * Wraps the stax cursor inteface (iterator just adds weight here).
 *
 * scala.xml.pull uses a thread based on stax approach to push/pull events.
 *
 * This code uses stax only, extra iteratee goodness will appear curtousy of scalaz....
 *
 */
trait XmlPulls {
}

