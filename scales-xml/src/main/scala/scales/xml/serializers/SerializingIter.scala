package scales.xml.serializers

import scalaz.Monad
import scalaz.iteratee.Input.Eof
import scalaz.iteratee.Iteratee.iterateeT
import scalaz.iteratee.StepT.{Cont, Done}
import scalaz.iteratee.{Input, IterateeT, StepT}
import scales.utils._
import scales.utils.resources._
import scales.xml._

/**
 * Provide push based serializing Iteratee
 */ 
trait SerializingIter {

  type SerialIterT[F[_]] = IterateeT[PullType, F, (XmlOutput, Option[Throwable])]
  type SerialStepT[F[_]] = StepT[PullType, F, (XmlOutput, Option[Throwable])]

  import java.nio.charset.Charset

  /**
   * The serializer will be returned automatically to the pool by calling closer
   *
   * doc functions are only evaluated upon the first elem / last elem
   *
   */
  def serializeIter[F[_]]( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc())(implicit F: Monad[F]) : SerialIterT[F] = {

    var empties = 0

    var theStatus: StreamStatus = null

    def done(status: StreamStatus): F[SerialStepT[F]] = {
      // give it back
      closer()

      F.point(Done((status.output, status.thrown), Eof[PullType]))
    }

    def rest(status: StreamStatus, prev: PullType)(s: Input[PullType]): SerialIterT[F] =
      iterateeT(
        s(el = e => {
            if (status.thrown.isDefined) done(status)
            else {
              val r = F.point{
                val r = StreamSerializer.pump((prev, e), status, serializer)
                r
              }

              F.map(r) {r=>
                theStatus = r
                Cont(rest(r, e))
              }
            }
          },
          empty = {
            empties += 1
            F.point(Cont(rest(status, prev)))
          },
          eof = {
            if (status.thrown.isDefined) done(status)
            else {
              val r = StreamSerializer.pump((prev, StreamSerializer.dummy), status, serializer)
              val opt = serializeMisc(r.output, doc.end.misc, serializer)._2

              val lastStatus = r.copy(thrown = opt)

              done(lastStatus)
            }
          }
        )
      )

    def first( status : StreamStatus)(s : Input[PullType]) : SerialIterT[F] =
      iterateeT(F.point(
        s(el = e => {
            // decl and prolog misc, which should have been collected by now
            val opt = serializer.xmlDeclaration(status.output.data.encoding,
              status.output.data.version).orElse{
              serializeMisc(status.output, doc.prolog.misc, serializer)._2
            }
            val nstatus = status.copy(thrown = opt)

            Cont(rest(nstatus, e))
          },
          empty = {
            empties += 1
            Cont(first(status))
          },
          eof = Done((status.output, Some(NoDataInStream())), Eof[PullType])
        )
      ))

    iterateeT(F.point(Cont(first(StreamStatus(output)))))
  }

  /**
   * Returns an Iteratee that can serialize PullTypes to out.  The serializer factory management is automatically handled upon calling with eof.  This can be triggered earlier by calling closeResource on the returned CloseOnNeed.
   */ 
  def pushXmlIter[F[_]]( out: java.io.Writer, doc : DocLike = EmptyDoc(), version: Option[XmlVersion] = None, encoding: Option[Charset] = None )(implicit serializerFI: SerializerFactory, F: Monad[F]) : (CloseOnNeed, SerialIterT[F]) = {

    val decl = doc.prolog.decl
    val sd = SerializerData(out, 
      version.getOrElse(decl.version), 
      encoding.getOrElse(decl.encoding))

    val xo = XmlOutput(sd)(serializerFI)

    val ser = serializerFI.borrow( sd ) 

    val closer : CloseOnNeed = new CloseOnNeed {
      def doClose() {
      	serializerFI.giveBack(ser)
      }
    }
    val iter = serializeIter[F]( xo, ser, () => closer.closeResource, doc)

    (closer, iter)
  }

}
