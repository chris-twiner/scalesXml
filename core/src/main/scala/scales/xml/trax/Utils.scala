package scales.xml.trax

import javax.xml.namespace.NamespaceContext
import javax.xml.stream._
import scales.xml.ScalesXml._
import scales.xml._

object EmptyStreamLocation extends Location {
  val getCharacterOffset : Int = -1
  val getColumnNumber : Int = -1
  val getLineNumber : Int = -1
  val getPublicId : String = null : String 
  val getSystemId : String = null : String 
}

trait TNC extends NamespaceContext {
  val parent : TNC

  val ns : Array[(String, String)]
}

object EmptyNamespaceContext extends TNC {
 import scala.collection.JavaConversions._

 val parent = this
   
 val ns = Array[(String,String)]() 

 def getNamespaceURI( prefix : String) : String = ""
 def getPrefix( namespaceURI : String) : String = ""
 def getPrefixes(namespaceURI : String) : java.util.Iterator[String] = List().iterator 
 
}

/**
 * Provides a namespace context, needs pushing / popping etc
 */ 
object NamespaceContextFunctions {
  
  def newContext( prev : TNC, elem : Elem ) =  new TNC {
    import scala.collection.JavaConversions._

    val parent = prev

    val ns = preToNS.toArray

    lazy val preToNS = elem.attributes.flatMap( a => a.name.prefix.map( p => Option((p, a.name.namespace.uri)) ).getOrElse(None) ).toMap ++ elem.namespaces

    val nsToPre = preToNS.foldLeft(Map[String, List[String]]()){ 
      (x, y) =>
      x.updated(y._2, x.get(y._2).map(y._1 :: _ ).
	       getOrElse( List(y._1) ))
    }
    
    def getNamespaceURI( prefix : String) : String = preToNS.get(prefix).getOrElse(prev.getNamespaceURI(prefix))
    def getPrefix( namespaceURI : String) : String = nsToPre.get(namespaceURI).map(_.head).getOrElse(prev.getPrefix(namespaceURI))
    def getPrefixes(namespaceURI : String) : java.util.Iterator[String] = 
      nsToPre.get(namespaceURI).getOrElse(List()).iterator
  }

}
