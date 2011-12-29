package scales.xml



/**
 * Provides a key that also checks ===, used in caching where you
 * want to remove all collision possibilities.
 *
 * Note to reduce allocation costs its very mutable, use QName directly when
 * possible.  Note here the allocation costs refer more to the gc hit of the
 * extra allocations that are then directly thrown away.
 *
 * No QNames were hurt during the use of this class.
 */
final class FullEqualQNameKey() {
  
  // Some also adds creation cost
  var prefix : String = _
  var local : String = _
  var namespace : String = _  

  /**
   * Makes a copy suitable for caching.
   */ 
  @inline def copy = {
    val n = new FullEqualQNameKey()
    n.prefix = prefix
    n.local = local
    n.namespace = namespace
    n.lastHash = lastHash
    n
  }

  @inline def set( qname : QName ) = {
    prefix = qname.prefix.getOrElse(null:String)
    local = qname.local
    namespace = qname.namespace.uri
    lastHash = 0
    hashCode
    this
  }

  @inline def setNoNamespaceQName( nlocal : String ) = {
    prefix = null:String
    local = nlocal
    namespace = ""//Default.noNamespace
    lastHash = 0
    hashCode
    this
  }

  @inline def setUnprefixedQName( nlocal : String, nnamespace : String ) = {
    prefix = null:String
    local = nlocal
    namespace = nnamespace
    lastHash = 0
    hashCode
    this
  }

  @inline def setPrefixedQName( nlocal : String, nnamespace : String, nprefix : String ) = {
    prefix = nprefix
    local = nlocal
    namespace = nnamespace
    lastHash = 0
    hashCode
    this
  }

  def =:=( other : FullEqualQNameKey ) =
    if (this eq other) true
    else
    (other.local == local && namespace == other.namespace)

  override def equals( other : Any ) = other match {
    case oq : FullEqualQNameKey =>
      if (this eq oq) true
      else
      if ((oq.lastHash == lastHash) && (this =:= oq)) 
	if ((oq.prefix ne null) && (prefix ne null))
	  prefix == oq.prefix
	else
	  ((oq.prefix eq null) && (prefix eq null))
      else 
	false
    case _ => error("Cannot compare a FullEqualQNameKey to anything else other than a FullEqualQNameKey")
  }

  /**
   * If its non 0 then its the last caclulated hash since a setXX
   */ 
  var lastHash : Int = 0

  override def hashCode() : Int = {
    if (lastHash != 0) return lastHash

    var hs = 1
    hs = (hs * 31) + (
      if (prefix eq null) 1
      else prefix.hashCode
      )
    hs = (hs * 31) + local.hashCode
    hs = (hs * 31) + namespace.hashCode
    lastHash = hs
    hs
  }
}
