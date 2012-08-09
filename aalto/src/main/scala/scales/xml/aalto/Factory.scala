package scales.xml.aalto

import com.fasterxml.aalto._

import scales.xml._

/**
 * Default AsyncXMLInputFactory impl
 */ 
object AsyncXMLInputFactoryPool extends scales.utils.SimpleUnboundedPool[AsyncXMLInputFactory] { pool =>
  
  val cdata = "http://java.sun.com/xml/stream/properties/report-cdata-event"

  def create = {
    val fac = AsyncXMLInputFactory.newInstance()
    if (fac.isPropertySupported(cdata)) {
      fac.setProperty(cdata, java.lang.Boolean.TRUE);
    }
    fac
  }
										   }
