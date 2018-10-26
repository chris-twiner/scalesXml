package scales.utils.collection

import scales.utils.collection.array._

import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, SeqFactory}
import scala.collection.mutable.Builder
import scala.collection.{IndexedSeq, IndexedSeqOptimized}

object ImmutableArrayProxyBuilder {
  final val vectorAfter = 31
}

/**
 * Build array first then vector as needed
 */
case class ImmutableArrayProxyBuilder[ A ]() extends Builder[A, ImmutableArrayProxy[A]]{
  import ImmutableArrayProxyBuilder._

  private[this] var arrayBuilder = new ImmutableArrayBuilder[A]()
  private[this] var vectorBuilder : Builder[A,scala.collection.immutable.Vector[A]] = _
  
  private[this] var inVector = false
  private[this] var haveChosen = false

  /**
   * Can the arrayBuilder be re-used?
   */ 
  private[this] var canReuse = true

  // keep it locally
  private[this] var length = 0

  override def sizeHint( size : Int ) {
    if (size > vectorAfter) {
      inVector = true
      haveChosen = true
      if (vectorBuilder eq null) {
	vectorBuilder = Vector.newBuilder[A]
      }
    }

    if (inVector) {
      vectorBuilder.sizeHint(size)
    } else {
      arrayBuilder.sizeHint(size)
    }
  }

  // for the case when we were under 32 but are now over
  @inline final protected def checkVB() {
    if (!inVector) {
      if (length > vectorAfter) {
	moveToVector
      }
    }
  }

  protected def moveToVector() {
    // copy over
    val r = arrayBuilder.result
    if (vectorBuilder eq null) {
      vectorBuilder = Vector.newBuilder[A]
    }
    vectorBuilder.sizeHint(r.len)
    vectorBuilder.++=(r)
    arrayBuilder.clear
    inVector = true
    haveChosen = true
  }

  def result : ImmutableArrayProxy[A] =
    if (inVector) VectorImpl(vectorBuilder.result)
    else { // do it here as this is the correct type
      val buf = arrayBuilder.buf
      import scala.annotation.switch

      (length : @switch) match {
	case 0 =>
	  ImmutableArrayProxy.emptyImmutableArray.asInstanceOf[ImmutableArrayProxy[A]]
	case 1 =>
	  IAOne(buf(0).asInstanceOf[A])
	case 2 =>
	  IATwo(buf(0).asInstanceOf[A], buf(1).asInstanceOf[A])
	case 3 =>
	  IAThree(buf(0).asInstanceOf[A], buf(1).asInstanceOf[A], buf(2).asInstanceOf[A])
	case _ => {
	  canReuse = false
	  if (length != 0 && length == buf.length)
	    ImmutableArrayAll[A](buf)
	  else
	    arrayBuilder.result
	}
      }
    }

  override def ++=(xs: TraversableOnce[A]): this.type = {
    // if its already a vector don't start with arrays again
    if (!haveChosen && xs.isInstanceOf[VectorImpl[A]]) {
      inVector = true
      haveChosen = true
      if (vectorBuilder eq null) {
	vectorBuilder = Vector.newBuilder[A]
      }
    }
    
    xs match {
      case p : ImmutableArrayProxy[A] => 
	if (inVector) 
	  vectorBuilder.++=(p.ar) 
	else
	  arrayBuilder.++=(p.ar)
      case _ => 
	if (inVector)
	  vectorBuilder.++=(xs)
	else
	  arrayBuilder.++=(xs)
    }

    length += xs.size

    checkVB
    this
  }

  def +=( elem : A) : this.type = {
    length += 1
    if (inVector) {
      vectorBuilder.+=(elem)
      this
    } else {
      arrayBuilder.+=(elem)
      checkVB
      this
    }
  }

  def clear() {
    if (inVector) { // a bit more expensive then resetting the len
      vectorBuilder.clear
    }
    if (!canReuse) {
      // array was used but not a vector or IAXxxs
      arrayBuilder = new ImmutableArrayBuilder[A]()
    } else {
      arrayBuilder.clear
    }
    inVector = false
    haveChosen = false
    canReuse = true
    length = 0
  }
}

/**
 * Wraps behaviour of ImmutableArray like objects, when the array is greater than 31 it will be swapped to Vector.
 *
 */ 
trait ImmutableArrayProxy[+A] extends IndexedSeq[A] with IndexedSeqOptimized[A, ImmutableArrayProxy[A]] with GenericTraversableTemplate[A, ImmutableArrayProxy] {

  @inline override def companion: GenericCompanion[ImmutableArrayProxy] = ImmutableArrayProxy

  override protected[this] def newBuilder: Builder[A, ImmutableArrayProxy[A]] = ImmutableArrayProxy.newBuilder[A]

  def ar : TraversableOnce[A]
 
}

/**
 * Starts an ImmutableArrayProxy and provides the CanBuildFrom
 */ 
object ImmutableArrayProxy extends SeqFactory[ImmutableArrayProxy] {
  val emptyImmutableArray = IAEmpty[Nothing]()

  // implement directly improved apply (switch and directly build)

  /**
   * Convenience constructor creates a single cell array
   */ 
  def one[A](a: A): ImmutableArrayProxy[A] = IAOne(a)

  @inline final override def empty[ A ] : ImmutableArrayProxy[A]  = emptyImmutableArray.asInstanceOf[ImmutableArrayProxy[A]]

  @inline def newBuilder[A]
  : Builder[A, ImmutableArrayProxy[A]] =
    ImmutableArrayProxyBuilder()
  
  @inline implicit def canBuildFrom[T](implicit ma: ClassManifest[T]): CanBuildFrom[ImmutableArrayProxy[_], T, ImmutableArrayProxy[T]] = new ImmutableArrayProxyCBF[T]{ val m = ma }

  trait ImmutableArrayProxyCBF[T] extends CanBuildFrom[ImmutableArrayProxy[_], T, ImmutableArrayProxy[T]] {
    
    val m : ClassManifest[T]

    def apply(from: ImmutableArrayProxy[_]): Builder[T, ImmutableArrayProxy[T]] = newBuilder
    def apply: Builder[T, ImmutableArrayProxy[T]] = newBuilder
  }
    
}
