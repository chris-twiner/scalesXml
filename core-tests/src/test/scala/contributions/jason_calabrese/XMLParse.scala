package contributions.jason_calabrese

import java.io.InputStream

object XMLParse {

  //based on current perf testing parseXMLPullMutable it the fastest and also has good memory use
  def parseXML(stream: InputStream): Iterator[Map[String, List[String]]] = parseXMLPullMutable(stream)
  
  def parseXMLPullDSL(stream: InputStream): Iterator[Map[String, List[String]]] = {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(streamToSource(stream))

    for {
      reference <- iterate(List("references"l, "reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- (reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))

  }

  def parseXMLTree(stream: InputStream): Iterator[Map[String, List[String]]] = {
    import scala.xml.{Elem, XML}

    val xml = XML.load(stream)
    
    (xml \\ "reference").toIterator.map { ref =>
      ref.child.toList.collect {
        case child: Elem => (child.label, child.text)
      }.groupBy(_._1.toUpperCase).mapValues(_.map(_._2))
    }

  }

  def parseXMLPullMutable(stream: InputStream): Iterator[Map[String, List[String]]] = {
    import scales.xml._, ScalesXml._
    import scala.collection.mutable.ListBuffer

    var inRef = false
    var currentField: String = null
    val currentRef = new ListBuffer[(String, String)]
    val refs = new ListBuffer[List[(String, String)]]

    for (pull <- pullXml(streamToSource(stream))) {
      pull match {
        case Left(event) => event match {
          case Elem(name, _, _) if name.local == "reference" =>
            inRef = true
          case Elem(name, _, _) if inRef =>
            currentField = name.local
          case text: Text if inRef && !text.value.trim.isEmpty =>
            currentRef.append((currentField, text.value))
          case other => //do nothing
        }
        case Right(end) if end.name.local == "reference" =>
          inRef = false
          refs.append(currentRef.toList)
          currentRef.clear
        case _ => //do nothing
      }
    }
    
    refs.toIterator.map(_.groupBy(_._1.toUpperCase).mapValues(_.map(_._2)))

  }

}
