package scales.xml.serializers

import scales.xml._
import scales.utils._

object StreamSerializer {
  import ScalesXml._

  val dummy = Left(Elem("dummy"l))
  val dummyIterable : Iterable[PullType] = List(dummy)

}

/**
 * Provides a base class for steam handling
 */ 
class StreamSerializer[T](toP : T => Iterator[PullType]) extends SerializeableXml[T] {
  import StreamSerializer._ 
  
  /**
   * Override to provide an actual doc
   */ 
  def doc(it: T) : DocLike = EmptyDoc()
  def apply(itT: T)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = {
    // 2.10 forces implicits at copy use site NOT tt declaration site
    import out.serializerF

    val it = toP(itT)
    // left of the sequence is our actual, 
    val r = it.++(dummyIterable.iterator).sliding(2).foldLeft((out, None: Option[Throwable], false)) { (cur, two) =>
      val (output, y, isEmpty) = cur
      if (y.isDefined) cur
      else {
	val asList = two.toList
	if (!((asList.size == 1) && (asList.head eq dummy))) { // only == 1 with an empty input Iterator
	  
	  val List(ev, next) = asList
	  ev match {
	    case Left(i: XmlItem) =>
	      (output, serializer.item(i, output.path), false)
	    case Left(x: Elem) =>
	      // if next is an end elem then its an empty
	      if (next.isRight) {

		// x.namespaces can't be used any further
		val nc = doElement(x, output.currentMappings.top)
		(output, serializer.emptyElement(x.name, x.attributes, nc.declMap, nc.addDefault, x.name :: output.path), true) // let us know to ignore the next end
	      } else {
		val npath = x.name :: output.path

		val nc = doElement(x, output.currentMappings.top)
		(output.copy(currentMappings = output.currentMappings.push(nc.mappings), path = npath),
		 serializer.startElement(x.name, x.attributes, nc.declMap, nc.addDefault, npath), false)
	      }
	    case Right(endElem) =>
	      if (isEmpty)
		(output, None, false)
	      else
		// pop the last ones
		(output.copy(currentMappings = output.currentMappings.pop,
		    path = output.path.tail), 
		   serializer.endElement(endElem.name, output.path), false)

	  }
	} else (output, Some(NoDataInStream()), true)
      }
    }
    (r._1, r._2)
  }
}

/**
 * SerializeableXml instances for the core types
 */ 
trait SerializerImplicits {

  /**
   * Serializes an XmlTree
   */
  implicit val treeSerializeable: SerializeableXml[XmlTree] = new SerializeableXml[XmlTree] {
    def doc(it: XmlTree) = Doc(it)
    def apply(it: XmlTree)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = {
      // 2.10 forces implicits at copy use site NOT tt declaration site
      import out.serializerF

      fold[XmlItem, Elem, XCC, (XmlOutput, Option[Throwable])]((out, None)) { (iorsw, pair) =>
        val (output, y) = pair

        if (y.isDefined) pair
        else
          iorsw match {
            case Left(item) => (output, serializer.item(item, output.path))
            case Right(walk) =>
              val x = walk.section
              if (!walk.hasChildren) {

                // x.namespaces can't be used any further
                val nc = doElement(x, output.currentMappings.top)
                (output, serializer.emptyElement(x.name, x.attributes, nc.declMap, nc.addDefault, x.name :: output.path))
              } else {
                if (walk.isStart) {

                  val npath = x.name :: output.path

                  val nc = doElement(x, output.currentMappings.top)
                  (output.copy(currentMappings = output.currentMappings.push(nc.mappings),
                    path = npath),
                    serializer.startElement(x.name, x.attributes, nc.declMap, nc.addDefault, npath))
                } else {
                  // pop the last ones
                  (output.copy(currentMappings = output.currentMappings.pop,
                    path = output.path.tail), serializer.endElement(x.name, output.path))
                }
              }
          }
      }(it)
  }}

  /**
   * Serializes a DslBuilder
   */
  implicit val builderSerializeable : SerializeableXml[DslBuilder] = new SerializeableXml[DslBuilder] {
    def doc(it: DslBuilder) = Doc(it.toTree)
    def apply(it: DslBuilder)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(it.toTree)(out, serializer)
  }

  /**
   * Serializes a Doc (Wrapped XmlTree)
   */
  implicit val docSerializeable : SerializeableXml[Doc]= new SerializeableXml[Doc] {
    def doc(it: Doc) = it
    def apply(it: Doc)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(it.rootElem)(out, serializer)
  }
  
  /**
   * Simple Elem serializer
   */
  implicit val elemSerializable : SerializeableXml[Elem] = new SerializeableXml[Elem] {
    def doc(it: Elem) = Doc(DslBuilder.elem2tree(it))
    def apply(it: Elem)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(DslBuilder.elem2tree(it))(out, serializer)
  } 




  /**
   * Serializes an Xml Stream
   */
  implicit val streamSerializeable: SerializeableXml[Iterator[PullType]] = new StreamSerializer(identity)

  /**
   * Serializes an XmlPull
   */
  implicit val pullCloseableOnlySerializeable: SerializeableXml[CloseablePull] = new StreamSerializer[CloseablePull](identity){
    override def doc(it: CloseablePull) = it
  }

  /**
   * Serializes an XmlPull Resource
   */
  implicit val pullOnlySerializeable: SerializeableXml[XmlPull] = new StreamSerializer[XmlPull](identity) {
    override def doc(it: XmlPull) = it
  }

  /**
   * Serializes an Iterator and DocLike
   */
  implicit val pullAndDocSerializeable: SerializeableXml[(Iterator[PullType], DocLike)] = new StreamSerializer[(Iterator[PullType], DocLike)](_._1) {
    override def doc(it: (Iterator[PullType], DocLike)) = it._2
  }


}
