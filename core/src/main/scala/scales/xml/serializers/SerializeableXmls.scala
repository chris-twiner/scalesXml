package scales.xml.serializers

import scales.xml._
import scales.utils._

/**
 * SerializeableXml instances for the core types
 */ 
trait SerializerImplicits {

  /**
   * Serializes an XmlTree
   */
  implicit val treeSerializeable: SerializeableXml[XmlTree] = new SerializeableXml[XmlTree] {
    def doc(it: XmlTree) = Doc(it)
    def apply(it: XmlTree)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) =

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
                val (mappings, (attribs, declMap, addDef)) = doElement(x, output)
                (output, serializer.emptyElement(x.name, attribs, declMap, addDef, x.name :: output.path))
              } else {
                if (walk.isStart) {

                  val npath = x.name :: output.path

                  val (mappings, (attribs, declMap, addDef)) = doElement(x, output)

                  (output.copy(currentMappings = output.currentMappings.push(mappings),
                    path = npath),
                    serializer.startElement(x.name, attribs, declMap, addDef, npath))
                } else {
                  // pop the last ones
                  (output.copy(currentMappings = output.currentMappings.pop,
                    path = output.path.tail), serializer.endElement(x.name, output.path))
                }
              }
          }
      }(it)
  }

  /**
   * Serializes a DslBuilder
   */
  implicit val builderSerializeable = new SerializeableXml[DslBuilder] {
    def doc(it: DslBuilder) = Doc(it.toTree)
    def apply(it: DslBuilder)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(it.toTree)(out, serializer)
  }

  /**
   * Serializes a Doc (Wrapped XmlTree)
   */
  implicit val docSerializeable = new SerializeableXml[Doc] {
    def doc(it: Doc) = it
    def apply(it: Doc)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(it.rootElem)(out, serializer)
  }
  
  /**
   * Simple Elem serializer
   */
  implicit val elemSerializable = new SerializeableXml[Elem] {
    def doc(it: Elem) = Doc(DslBuilder.elem2tree(it))
    def apply(it: Elem)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = treeSerializeable(DslBuilder.elem2tree(it))(out, serializer)
  } 

  /**
   * Serializes an Xml Stream
   */
  implicit val streamSerializeable: SerializeableXml[Iterator[PullType]] = new SerializeableXml[Iterator[PullType]] {

    import ScalesXml._

    val dummy: Iterable[PullType] = List(Left(Elem("dummy"l)))

    def doc(it: Iterator[PullType]) = EmptyDoc()
    def apply(it: Iterator[PullType])(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = {
      // left of the sequence is our actual, 
      val r = it.++(dummy.iterator).sliding(2).foldLeft((out, None: Option[Throwable], false)) { (cur, two) =>
        val (output, y, isEmpty) = cur
        if (y.isDefined) cur
        else {
          val List(ev, next) = two.toList
          ev match {
            case Left(i: XmlItem) =>
              (output, serializer.item(i, output.path), false)
            case Left(x: Elem) =>
              // if next is an end elem then its an empty
              if (next.isRight) {

                // x.namespaces can't be used any further
                val (mappings, (attribs, declMap, addDef)) = doElement(x, output)
                (output, serializer.emptyElement(x.name, attribs, declMap, addDef, x.name :: output.path), true) // let us know to ignore the next end
              } else {
                val npath = x.name :: output.path

                val (mappings, (attribs, declMap, addDef)) = doElement(x, output)

                (output.copy(currentMappings = output.currentMappings.push(mappings), path = npath),
                  serializer.startElement(x.name, attribs, declMap, addDef, npath), false)
              }
            case Right(endElem) =>
              if (isEmpty)
                (output, None, false)
              else
                // pop the last ones
                (output.copy(currentMappings = output.currentMappings.pop,
                  path = output.path.tail), serializer.endElement(endElem.name, output.path), false)

          }
        }
      }
      (r._1, r._2)
    }
  }

  /**
   * Serializes an XmlPull
   */
  implicit val pullCloseableOnlySerializeable: SerializeableXml[CloseablePull] = new SerializeableXml[CloseablePull] {
    def doc(it: CloseablePull) = it
    def apply(it: CloseablePull)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = streamSerializeable(it)(out, serializer)
  }

  /**
   * Serializes an XmlPull Resource
   */
  implicit val pullOnlySerializeable: SerializeableXml[XmlPull] = new SerializeableXml[XmlPull] {
    def doc(it: XmlPull) = it
    def apply(it: XmlPull)(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = streamSerializeable(it)(out, serializer)
  }

  /**
   * Serializes an Iterator and DocLike
   */
  implicit val pullAndDocSerializeable: SerializeableXml[(Iterator[PullType], DocLike)] = new SerializeableXml[(Iterator[PullType], DocLike)] {
    def doc(it: (Iterator[PullType], DocLike)) = it._2
    def apply(it: (Iterator[PullType], DocLike))(out: XmlOutput, serializer: Serializer): (XmlOutput, Option[Throwable]) = streamSerializeable(it._1)(out, serializer)
  }


}
