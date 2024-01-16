package scales.xml.serializers

import scales.utils._
import resources._
import scalaz.Id.Id
import scalaz.Monad
import scalaz.iteratee.Input.Eof
import scalaz.iteratee.Iteratee.{iteratee, iterateeT}
import scalaz.iteratee.StepT.{Cont, Done}
import scales.xml._
import scalaz.iteratee.{Enumerator, Input, Iteratee, IterateeT, StepT}
import scales.utils.iteratee.functions.{ResumableIter, ResumableStep}

/**
 * Provide push based serializing Iteratee
 */ 
trait SerializingIter {

  type SerialIterT[F[_]] = ResumableIter[PullType, F, (XmlOutput, Option[Throwable])]
  type SerialStepT[F[_]] = ResumableStep[PullType, F, (XmlOutput, Option[Throwable])]

  import java.nio.charset.Charset

  /**
   * The serializer will be returned automatically to the pool by calling closer
   *
   * doc functions are only evaluated upon the first elem / last elem
   *
  def serializeIter[F[_]]( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc())(implicit F: Monad[F]) : SerialIterT[F] = {
    // cannot pass it on the stack as we'll get odd ordering with trampolines
    var status: StreamStatus = StreamStatus(output)

    var prevE: PullType = null

    def shouldPump(e: PullType): Option[PullType] =
      if (e ne prevE) {
        prevE = e
        Some(e)
      } else
        // Duplicates happen when restarting the processing in the face of trampolines.  Using run without asynchronous empties does not suffer this issue
        None

    var empties = 0

    def done(status: StreamStatus): F[SerialStepT[F]] = {
      // give it back
      closer()
      //println("empties was "+empties)
      F.point(Done((status.output, status.thrown), Eof[PullType]))
    }

    def go(serializer: Serializer)(s: Input[PullType]): SerialIterT[F] =
      iterateeT(
        s(el = e => {
          if (status.thrown.isDefined) done(status)
          else {
            val r = shouldPump(e)
            F.point(
              r.fold(Cont(go(serializer))) { e =>
                val nstatus =
                  if (!status.haveSetProlog) {
                    // decl and prolog misc, which should have been collected by now
                    val opt = serializer.xmlDeclaration(status.output.data.encoding,
                      status.output.data.version).orElse {
                      serializeMisc(status.output, doc.prolog.misc, serializer)._2
                    }

                    status.copy(thrown = opt, haveSetProlog = true)
                  } else
                    status

                val r = StreamSerializer.pump(e, nstatus, serializer)
                status = r

                //println("pumped rest e " + System.identityHashCode(e) + " r " + System.identityHashCode(r))
                Cont(go(serializer))
              }
            )
          }
        },
          empty = {
            empties += 1
            //println("outitr empty " +System.identityHashCode(prev))
            F.point(Cont(go(serializer)))
          },
          eof = {
            if (status.thrown.isDefined) done(status)
            else {
              val r = StreamSerializer.pump(StreamSerializer.EOF, status, serializer)
              val opt = serializeMisc(r.output, doc.end.misc, serializer)._2

              val lastStatus = r.copy(thrown = opt)

              done(lastStatus)
            }
          }
        )
      )

    iterateeT(F.point(Cont(go(serializer))))
  }
*/
  def serializeIter[F[_]]( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc())(implicit F: Monad[F]) : SerialIterT[F] = {

    var empties = 0

    def done(status: StreamStatus, cont: SerialIterT[F]): F[SerialStepT[F]] = {
      // give it back
      closer()
      //println("empties was "+empties)
      F.point(Done(((status.output, status.thrown), cont), Eof[PullType]))
    }

    var theStatus: StreamStatus = null

    def go(fromStart: Boolean, status: StreamStatus)(s: Input[PullType]): SerialIterT[F] =
      iterateeT(
        s(el = e => {
            if (status.thrown.isDefined) done(status, iterateeT(F.point(Cont(go(false, status)))))
            else {
              val r = F.point{
                val r = StreamSerializer.pump(e, status, serializer)
                println("pumped rest e " + System.identityHashCode(e) + " r " + System.identityHashCode(r) + " status prev "+ status.prev +" r.prev "+ r.prev)
                r
              }

              F.map(r) {r=>
                theStatus = r
                val f = go(false, r)_
                Cont(f)
              }
            }
          },
          empty = {
            empties += 1
            val nstatus = if (theStatus eq null) status else theStatus
            println("outitr empty " +nstatus.prev + " from start " + fromStart )
            val f = go(false, nstatus) _
            F.point(Cont(f))
          },
          eof = {
            if (status.thrown.isDefined) done(status, iterateeT(F.point(Cont(go(false, status)))))
            else {
              val r = StreamSerializer.pump(StreamSerializer.EOF, status, serializer)
              val opt = serializeMisc(r.output, doc.end.misc, serializer)._2

              val lastStatus = r.copy(thrown = opt)

              done(lastStatus, iterateeT(F.point(Cont(go(false, lastStatus)))))
            }
          }
        )
      )

    lazy val status = {
      val status = StreamStatus(output)
      if (!status.haveSetProlog) {
        // decl and prolog misc, which should have been collected by now
        val opt = serializer.xmlDeclaration(status.output.data.encoding,
          status.output.data.version).orElse {
          serializeMisc(status.output, doc.prolog.misc, serializer)._2
        }

        status.copy(thrown = opt, haveSetProlog = true)
      } else
        status
    }

    iterateeT(F.point(Cont(go(true, status))))
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
