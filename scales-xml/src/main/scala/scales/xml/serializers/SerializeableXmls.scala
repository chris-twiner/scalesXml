package scales.xml.serializers

import scales.xml._

import dsl.DslBuilder

import scales.utils._

/**
 * Status of stream processing
 */
case class StreamStatus(output : XmlOutput, thrown: Option[Throwable] = None, isEmpty : Boolean = false, prev: Option[PullType] = None, haveSetProlog: Boolean = false)

/**
 * Provides basis for serialisation of streams.
 */ 
object StreamSerializer {
  import ScalesXml._

  val EOF = Left(Elem("EEEEOFFF" l))
  val dummy = Left(Elem("dummy"l))
  val dummyIterable : Iterable[PullType] = List(dummy)

  /**
   * Pushes a single PullType to the given output, requires pairing of events
  def pump(event: (PullType, PullType), out: XmlOutput, serializer: Serializer) : (XmlOutput, Option[Throwable]) = {
   */

  /**
   * Pushes a single PullType to the given output, requires a finishing "dummy" event to indicate the end of the document.
   * If status.thrown is defined status is returned
   */
  def pump(currentEvent: PullType, status : StreamStatus, serializer: Serializer) : StreamStatus = {
    // shadow it
    import status.output.{serializerF => defaultSerializerFactory}
    import status.{output, isEmpty}

    if (status.thrown.isDefined) status
    else {

      (status.prev, currentEvent) match {
        case (None, EOF) =>
          status.copy(thrown = Some(NoDataInStream()))
        case (None, e) =>
          status.copy(prev = Some(currentEvent))
        case (Some(Right(lastElem)), EOF) if status.isEmpty =>
          status // nothing to do
        case (Some(Right(lastElem)), EOF) =>
          status.copy(output.copy(currentMappings = output.currentMappings.tail,
            path = Nil),
            serializer.endElement(lastElem.name, output.path), false, Some(currentEvent))
        case (Some(Left(p: XmlItem)), _) => // allow EOF's through for xmlitems
          status.copy(output, serializer.item(p, output.path), false, Some(currentEvent))
        case (_, EOF) =>
          status.copy(thrown = Some(new IllegalStateException("EOF should only be sent after the closing element")))
        case (Some(Right(endElem)), Left(i: XmlItem)) if !status.isEmpty => // don't do this for other end elements
          status.copy(output.copy(currentMappings = output.currentMappings.tail,
              path = output.path.tail),
              serializer.endElement(endElem.name, output.path), false, Some(currentEvent))
        case (Some(Right(endElem)), Left(_)) if status.isEmpty => // it's already closed
          status.copy(prev = Some(currentEvent))
        case (Some(Right(endElem)), Left(x: Elem)) if !status.isEmpty => // unrelated elements
          status.copy(output.copy(currentMappings = output.currentMappings.tail,
              path = output.path.tail),
              serializer.endElement(endElem.name, output.path), false, Some(currentEvent))
        case (Some(Left(x: Elem)), Right(endElem)) =>
          // must be an empty
          // x.namespaces can't be used any further
          val nc = doElement(x, output.currentMappings.head)
          status.copy(output, serializer.emptyElement(x.name, x.attributes, nc.declMap, nc.addDefault, x.name :: output.path), true, Some(currentEvent)) // let us know to ignore the next end
        case (Some(Left(x: Elem)), _) =>
          val npath = x.name :: output.path

          val nc = doElement(x, output.currentMappings.head)
          status.copy(output.copy(currentMappings = nc.mappings +: output.currentMappings, path = npath),
            serializer.startElement(x.name, x.attributes, nc.declMap, nc.addDefault, npath), false, Some(currentEvent))
      }
    }
  }
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
    // 2.10 forces implicits at copy use site NOT at declaration site
    import out.serializerF

    val it = toP(itT)
    // left of the sequence is our actual,
    val allButLast = it.foldLeft(StreamStatus(out)) {  (status, next) =>
      if (status.thrown.isDefined) status
      else {
        pump(next, status, serializer)
      }
    }
    // complete it
    val r = pump(EOF, allButLast, serializer)
    (r.output, r.thrown)
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
      // 2.10 forces implicits at copy use site NOT at declaration site
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
                val nc = doElement(x, output.currentMappings.head)
                (output, serializer.emptyElement(x.name, x.attributes, nc.declMap, nc.addDefault, x.name :: output.path))
              } else {
                if (walk.isStart) {

                  val npath = x.name :: output.path

                  val nc = doElement(x, output.currentMappings.head)
                  (output.copy(currentMappings = nc.mappings +: output.currentMappings,
                    path = npath),
                    serializer.startElement(x.name, x.attributes, nc.declMap, nc.addDefault, npath))
                } else {
                  // pop the last ones
                  (output.copy(currentMappings = output.currentMappings.tail,
                    path = output.path.tail), serializer.endElement(x.name, output.path))
                }
              }
          }
      }(it)
  } }

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
