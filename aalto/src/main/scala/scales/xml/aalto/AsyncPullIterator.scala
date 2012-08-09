package scales.xml.aalto

import scales.xml._
/*
trait AsyncPull extends XmlPullBase {

  private[AsyncPull] var currentEvent : Int = 

  override def next: PullType = {
    val c = current // cache current
    if (current eq null) throw new NoSuchElementException("The end of the document has been reached")

    current = pumpEvent // pump for the next
    if ((current ne null) && current.isRight && depth == -1) {
      // we are now into the end doc, no more events will be pumped
      var ends = pumpEvent
      while (ends ne null) {
        emisc = emisc.copy(misc = emisc.misc :+ getMisc(ends, "document end Misc"))
        ends = pumpEvent
      }
    }

    c // return cached
  }

  /**
   * if its 
   */
  final def otherEventHandler( event : Int ) : PullType =
    pumpEvent // final - let the jvm inline it
}
*/
