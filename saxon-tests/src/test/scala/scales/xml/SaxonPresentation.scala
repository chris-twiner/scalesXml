package scales.xml

import scales.utils._
import ScalesUtils._
import scales.xml._
import ScalesXml._

import collection.path.Replace

/**
 * Code used in the intro presentation.  Variables aren't re-used as the functions
 * are meant to standalone as piccies in the pressy.
 */ 
object SaxonPresentation {

  def separation {
    val unprefixedQName = "uri:namespace" :: "localName"
    val elem = Elem(unprefixedQName)
    
    val root = <(elem) /( <(elem) )
    
    val doc = Doc(root)
    println("seperation " + asString(doc))
  }

  def qNames {
    val ns = Namespace("uri:namespace")
    val pre = ns.prefixed("pre")
    
    val unprefixedQName = ns("localName")
    val prefixedQName = pre("localName")
    // l is a NoNamespaceQName wrapper
    val nonamespaceQName = "localName"l

    val uelem = Elem(unprefixedQName)
    
    // type AttributeQName = 
    //  EitherLike[PrefixedQName, NoNamespaceQName]
    // implicitly converted
    val nonamespaceAQN : AttributeQName = 
      nonamespaceQName

    // won't compile
    //val unprefixedAQN  : AttributeQName = 
    //  unprefixedQName

    val root = 
      <(uelem) /@(Attribute(nonamespaceAQN,"nv"), 
		  prefixedQName -> "pv") /(
	prefixedQName, 
	nonamespaceQName 
      )
    
    println("qNames " + asString(root))
  }

  def typeSystemPII {
    val pre = Namespace("uri:test").prefixed("pre")

    val elem = Elem("fred"l, emptyAttributes + 
		    ("attr", "value") +
		    Attribute(pre("attr"), "value"))
    println("typeSystemPII: ")
    println("elem " + asString(<(elem)))

    println("attributes are a map " + elem.attributes("attr"))

    println("attributes are a set " + (
      elem.attributes + ("attr", "new value")))
    
    val xpath = top(<(elem)). \.*@ (pre("attr"))

    println("XPaths are a DSL ")
    xpath foreach{ap => println(ap.name)}
  }

  def consistentSyntax {
    val attributesAndChildren = <("uri:dsl" :: "ln") /@(
      "attr"->"nv") /("subchild"l, <("another"l) ~> "value")
    
    println("consistent with " + asString(attributesAndChildren))

    val without = attributesAndChildren -/@("attr") -/("subchild"l)
    
    println("consistent without " + asString(without))
  }

  def matching {
    val ns = Namespace("uri:namespace")
    val oqn = ns{"local"}
    val OQNM = oqn.m

    val elem = Elem(oqn, emptyAttributes + 
		    ("fred"->"value") + 
		  ("jane"->"another value"))

    // matching against just the qname
    elem match {
      case OQNM(e) => println("matching Our QName"); 1
      case _ => error("matching didn't")
    }

    // matched attributes are provided in order,
    // all must match
    val Matcher = ElemMatcher(oqn, "fred", "jane")
    elem match {
      case Matcher( elem, Attr(fredsValue) :: janesAttribute :: Nil) =>
	println("Matching - Our whole Elem "+asString(<(elem)))
	println("Matching - fredsValue "+ fredsValue)
	println("Matching - janesAttribute " + janesAttribute)
      case _ => error("matching didn't on Matcher")
    }
  }

  def traxCompatibility = {
    val elem = Elem("trax"l)
    val doc = Doc(<(elem) /( <(elem) ))

    import javax.xml.transform._
    val trax = TransformerFactory.
      newInstance.newTransformer

    val wr = new java.io.StringWriter()
    val str = new stream.StreamResult(wr)
    trax.transform(doc, str)
    println("trax")
    println("source only " + wr.toString)
    
    val sr = scales.xml.trax.ScalesResult()
    trax.transform(doc, sr)
    
    println("roundtrip " + asString(sr.doc))
  }

  // http://stackoverflow.com/questions/970675/scala-modifying-nested-elements-in-xml

  def transformations {
    def bits(i : Int) : Stream[XmlTree] = 
      Stream[XmlTree]( <("subnode"l) ~> i.toString ).append( 
	Stream[XmlTree](<("contents"l) ~> i.toString toTree) 
      ).append( bits( i + 1 ) )

    val builder = <("root"l) /( bits(0).take(6) )
    println("transformations "+ asString(builder))

    val subnodes = top(builder).\\.*("subnode"l)
    val folded = foldPositions( subnodes )( p => 
      Replace( p.tree ~> ((normalizeSpace( p ).toInt + 1).toString) toTree ) )

    val newRoot = folded.left.get
    println("transformations "+ asString(newRoot.tree))
  }

  def directPullParsing {
    val doc = Doc(<("root"l) /( <("subnode"l) ~> "1" ))
    val str = asString(doc)
    import javax.xml.transform._
        
    val xml = pullXml(new 
      java.io.StringReader(str))
    
    println("direct PullParsing ")
    for{ e <- xml } {
      e.fold( _ match {
	case Elem(qname, attribs, ns) => 
	  print(qname.local+" {")
	case Text(text) => print(text)
      }, end => print("}"))
    }
    println
  }

  def iterateEg {
    val pull = pullXml(new java.io.
		  FileReader("./src/test/data/svnLogIteratorEg.xml"))

    // here is the cool bit, only this repeating path is interesting
    val LogEntries = List("log"l,"logentry"l)

    // iterate is lazy by default
    val bits = for{ entry : XmlPath <- iterate(LogEntries, pull)
	revision <- entry.\.*@("revision"l).one
	author <- entry.\*("author"l).one
	path <- entry.\*("paths"l).\*("path"l)
	kind <- path.\.*@("kind"l)
	action <- path.\.*@("action"l)
    } yield (text(revision), 
	     value(author), text(kind), 
	     text(action), value(path))

    println("iterate ")
    bits drop 40 take 2 foreach println
    pull.close // we haven't read it all
  }

  def foldNames {
    val pull = pullXml(new java.io.
		  FileReader("./src/test/data/svnLogIteratorEg.xml"))

    // Who touched what file at what revision
    val LogEntries = List("log"l,"logentry"l)
    val Authors = LogEntries :+ ("author" localOnly)
    val Paths = LogEntries ++ List("paths"l, "path"l)

    // combine the Iteratee
    val ionDone = onDone(
      List(onQNames(Authors), 
	   onQNames(Paths))
    )
    
    val allAuthorsAndFiles = foldOnDone(iteratorEnumerator(pull.it))(
      ("", Map[String, List[String]]()), ionDone )  {
        case ((author, map), qnamesMatch) =>
          qnamesMatch match {
            case Nil => (author, map)
            case (Authors, Some(newAuthor)) :: Nil =>
              (value(newAuthor), map)
            case (Paths, Some(path)) :: Nil =>
              (author,
               map.updated(
           author,
           value(path) ::
             map.get(author).
             getOrElse(List())
               ))
            case _ => error("Oops " + qnamesMatch )
          }
    }
    
    allAuthorsAndFiles._2 take 2 foreach println
  }

  def main(args: Array[String]) {
    separation
    qNames
    typeSystemPII
    consistentSyntax
    matching
    traxCompatibility
    transformations
    directPullParsing
    iterateEg
    println("foldNames")
    foldNames
  }
}
