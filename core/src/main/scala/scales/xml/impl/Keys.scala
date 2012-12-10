package scales.xml.impl

import scales.xml.{QName, Attributes, emptyAttributes, emptyNamespaces}

object DefaultHashes {
  
  val emptyAttributesHash = emptyAttributes.hashCode
  val emptyNamespacesHash = emptyNamespaces.hashCode

}

import DefaultHashes._

/**
 * Allows quick lookups without creating elements, only use via copy and then don't change it
 */ 
class ElemKey {
  var name : QName = _
  var nameHash : Int = 0// incase it can be passed in
  var attributes : Attributes = _
  var namespaces : Map[String, String] = _

  def set(name : QName, attributes : Attributes, namespaces : Map[String, String], nameHash : Int = 0) = {
    this.name = name
    this.attributes = attributes
    this.namespaces = namespaces

    if (nameHash == 0)
      this.nameHash = name.hashCode
    else
      this.nameHash = nameHash
    
    lastHash = 0
    // do it now
    hashCode
    this
  }

  def copy = {
    val r = new ElemKey
    r.name = name
    r.attributes = attributes
    r.namespaces = namespaces
    r.lastHash = lastHash
    r
  }

  /**
   * Why are we doing eq's as well? Defaults and caching, and having to recast every time when we already know what we are..
   */ 
  override def equals( other : Any ) = other match {
    case oq : ElemKey =>
      if ((lastHash == oq.lastHash) &&
	((name eq oq.name) || (name ==== oq.name)) && 
	  ((attributes eq oq.attributes) || (attributes == oq.attributes)) &&
	  ((namespaces eq oq.namespaces) || (namespaces == oq.namespaces)) // only checks after running equals == instanceof checks ++
	)
	true
      else
	false
    case _ => scales.utils.error("Cannot compare an ElemKey to anything else other than a ElemKey")
  }

  /**
   * When non 0 it has been calculated
   */ 
  var lastHash : Int = 0

  override def hashCode() : Int = {
    if (lastHash != 0) return lastHash

    var hs = 1
    hs = (hs * 31) + nameHash
    hs = (hs * 31) + (
      if (emptyAttributes eq attributes)
	emptyAttributesHash // don't do it more than once
      else
	attributes.hashCode
      )
    hs = (hs * 31) + (
      if (namespaces eq emptyNamespaces)
	emptyNamespacesHash // don't do it more than once
      else
	namespaces.hashCode
      )

    lastHash = hs
    hs    
  }
}
