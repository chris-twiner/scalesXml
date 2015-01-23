package scales.iterv

import scales.utils._

import resources._

import scales.xml._

import serializers._

import java.io.Writer

import scalaz.{IterV, Enumerator, Input, EphemeralStream}
import scalaz.IterV._

/**
 * Provide push based serializing Iteratee
 */ 
trait SerializingIter {

  type SerialIterT = IterV[PullType, (XmlOutput, Option[Throwable])] 

  import java.nio.charset.Charset

  /**
   * The serializer will be returned automatically to the pool by calling closer
   * 
   * doc functions are only evaluated upon the first elem / last elem
   */
  def serializeIter( output : XmlOutput, serializer : Serializer, closer : () => Unit, doc : DocLike = EmptyDoc()) : SerialIterT = {

    var empties = 0

    def done( status : StreamStatus ) : SerialIterT = {
      // give it back
      closer()
      //println("empties was "+empties)
      Done((status.output, status.thrown), EOF[PullType])
    }

    def rest( status : StreamStatus, prev : PullType, serializer : Serializer )(s : Input[PullType]) : SerialIterT = {
      s(el = e => {
	if (status.thrown.isDefined) done(status)
	else {
	  val r = StreamSerializer.pump((prev, e), status, serializer)
	  if (r.thrown.isDefined) done(r)
	  else Cont(rest(r, e, serializer))
	}
	},
        empty = {
	  empties += 1
	  //println("outitr empty")
	  Cont(rest(status, prev, serializer))
	},
        eof =  {
	if (status.thrown.isDefined) done(status)
	else {
	  val r = StreamSerializer.pump((prev, StreamSerializer.dummy), status, serializer)
	  val opt = serializeMisc(r.output, doc.end.misc, serializer)._2
	    
	  val lastStatus = r.copy(thrown = opt)
	  
	  done(lastStatus)
	}})
    }
    
    def first( status : StreamStatus, serializer : Serializer )(s : Input[PullType]) : SerialIterT =
      s(el = e => {
	// decl and prolog misc, which should have been collected by now
	val opt = serializer.xmlDeclaration(status.output.data.encoding, 
				  status.output.data.version).orElse{
	    serializeMisc(status.output, doc.prolog.misc, serializer)._2
	  }
	val nstatus = status.copy(thrown = opt)
	  
	Cont(rest(nstatus, e, serializer))
	},
        empty = {
	  empties += 1
	  Cont(first(status, serializer))
	},
        eof = Done((status.output, Some(NoDataInStream())), EOF[PullType]))

    Cont(first(StreamStatus(output), serializer))
  }

  /**
   * Returns an Iteratee that can serialize PullTypes to out.  The serializer factory management is automatically handled upon calling with eof.  This can be triggered earlier by calling closeResource on the returned CloseOnNeed.
   */ 
  def pushXmlIter( out: java.io.Writer, doc : DocLike = EmptyDoc(), version: Option[XmlVersion] = None, encoding: Option[Charset] = None )(implicit serializerFI: SerializerFactory) : (CloseOnNeed, SerialIterT) = {

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
    val iter = serializeIter( xo, ser, () => closer.closeResource, doc)

    (closer, iter)
  }

}
