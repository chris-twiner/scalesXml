= Full XML Doc Parsing =

Parsing a full XML document in Scales can be very straightforward:

${cscala}
  val doc = loadXml(new FileReader("document.xml"))
${cend}

The input to loadXml is an InputSource, a PathOptimisationStrategy and a Loaner[SAXParser].  Defaults are provided for the strategy and Loaner, but can be overridden.

Conversions exist (pulled in via ScalesXml._) from InputStream, Readers and URL to ease the use of the api.

PathOptimisationStrategys allow the developer to tweak both the memory consumption and generation (and therefore the performance).  The default optimisation caches QNames across an application but does not attempt to cache elements or attributes.  Caching elements and attributes can lead to significant memory savings at the cost of parsing performance.  


As the names suggests PathOptimisationStrategies could also choose to optimise whole sub-trees, tests have not shown a general case where this is beneficial however (the cost of matching the tree typically outweighing potential memory savings).  See [./api.sxr/scales/xml/parser/strategies/NonDefaultStrategies.scala.html NonDefaultStrategies] for other optimisation options and traits with which to build your own.

Loaner is a simple interface to obtain SAXParser instances, other non default instances can be provided, such as JTagSoup or simply to allow customisations of SAX properties.  The default SAX parser pool also takes care of common threading issues.

== Direct SAX XMLReader Usage ==

As of Scales 0.3 there is direct support for SAX parsers via the loadXmlReader function.  This follows the same Loaner approach as the normal JAXP factories, for example:

${cscala}
  import org.xml.sax.XMLReader

  object NuValidatorFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with DefaultSaxSupport {
    
    def create = {
      import nu.validator.htmlparser.{sax,common}
      import sax.HtmlParser
      import common.XmlViolationPolicy

      val reader = new HtmlParser
      reader.setXmlPolicy(XmlViolationPolicy.ALLOW)
      reader.setXmlnsPolicy(XmlViolationPolicy.ALLOW)
      reader
    }      
    
  }    

  val xmlFile = resource(this, "/data/html.xml")
  val nuxml = loadXmlReader(xmlFile, parsers = NuValidatorFactoryPool)
${cend}

Developers can also call readXml directly with a given XMLReader instance instead of using the pooled version.

If a given XMLReader cannot work with the DefaultSaxSupport it can override the appropriate functions.  For example TagSoup doesn't support XmlVersion information, as such a TagSoupFactoryPool would look like:

${cscala}
object TagSoupFactoryPool extends scales.utils.SimpleUnboundedPool[XMLReader] with DefaultSaxSupport {
  
  // doesn't support xml version retrieval
  override def getXmlVersion( reader : XMLReader ) : AnyRef =
    null

  def create = {
    import org.ccil.cowan.tagsoup.Parser
    val reader = new Parser
    // disable namespaces
    reader.setFeature(Parser.namespacesFeature, false)
    reader
  }      
  
}
${cend}
