package scales.utils.resources

trait IsClosed {
  def isClosed : Boolean
}

/**
 * Mostly exists for pulling but it is general
 * 
 * We want to enable bracketing and a more orderly shutdown of resources from the input streams.
 * Whilst the resource is closed automatically its not very helpful if you don't want to close it.
 * Due to Sun bug: 6539065 we have to wrap the actual close.  This allows us to decide IF we want to close.
 *
 * So we cover three use cases here:
 * * Enumeratees can bracket, if so desired
 * * Streams of xml can be continuously plied for more xml messages
 * * Users who want to manually close early can do so too.
 *
 * As a note this fits very closely to the scala-arm stuff, which I happily use in another project.  But it has
 * two specific seperate use cases:
 * * XmlPulls should be joinable as iterators ++ should ensure that the resources are closed
 * * Additionally, however, closing on the resulting XmlPull should close the lot.
 * 
 * So we either override the ++ to behave differently or we abstract away using of the stream from closing it.
 *
 */ 
trait CloseOnNeed extends IsClosed { 
  parent =>

  protected def doClose : Unit 

  private[utils] var closed = false

  def isClosed = closed

  /**
   * Close the underlying something, but only do it once.
   *
   * This allows closing of an xml input stream directly after the end doc, but without disturbing
   * the normal model.
   */ 
  def closeResource =
    if (!closed) {
      closed = true;
      doClose
    } else ()
  
  def ++( close2 : CloseOnNeed ) : CloseOnNeed = new CloseOnNeed {
    added =>
  
    def doClose = ()
     
    override def closeResource = 
      if (!closed) {
        closed = true;
        parent.closeResource
        close2.closeResource
      } else ()

    // flip it back to stop from endlessly repeating ourselves
    override def ++(close3 : CloseOnNeed) = {
      close3 ++ this
    }
  }
}

/**
 * Simple pool interface
 */ 
trait Pool[T] {
  def grab : T 

  def giveBack( t : T ) : Unit
}

/**
 * Thread safe unbounded pool, if more objects are required it will simple create them.  The optional parameter reduceSize tries to help clean up a bit when an excessive amount is created but does not act as a semaphore
 */ 
trait SimpleUnboundedPool[T] extends Pool[T] with Loaner[T] with Creator[T] {
  val reduceSize : Int = 30
  
  val size = new java.util.concurrent.atomic.AtomicInteger(0)

  private[this] val cache = new java.util.concurrent.ConcurrentLinkedQueue[T]();

  def grab = {
    val res = cache.poll
    if (res != null) res
    else doCreate
  }

  def giveBack( t : T ) {
    if (size.get > reduceSize) size.decrementAndGet
    else cache.add(t)
  }

  final def doCreate = {
    size.getAndIncrement()
    create
  }

  /**
   * Performs a loan for you
   */ 
  def loan[X]( tThunk : T => X ) : X = {
    var t = null.asInstanceOf[T]
    try {
      t = grab // can throw
      tThunk(t)
    } finally {
      if (t != null) giveBack(t)
    }
  }

}

trait Loaner[T] {
  
  /**
   * Performs a loan for you
   */ 
  def loan[X]( tThunk : T => X ) : X 
}

/**
 * Simple factory interface
 */ 
trait Creator[T] {
  def create : T
}
