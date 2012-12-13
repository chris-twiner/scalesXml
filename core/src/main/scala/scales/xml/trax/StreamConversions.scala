package scales.xml.trax

import scales.utils._

import scales.xml.{PullType, XmlTree, impl => ximpl}

import ximpl.DslBuilder

/**
 * All safe conversions from T to Iterator[PullType].
 *
 */ 
trait PullTypeConversionImplicits {
  
  implicit def treeToStream( tree : XmlTree ) : Iterator[PullType] = new TreeIterable(tree)
  
  implicit def dslToStream( ds : DslBuilder ) : Iterator[PullType] = new TreeIterable(ds.toTree)
  
}
