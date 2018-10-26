package scales.xml.trax

import scales.xml.dsl.DslBuilder
import scales.xml.{PullType, XmlTree, dsl}

/**
 * All safe conversions from T to Iterator[PullType].
 *
 */ 
trait PullTypeConversionImplicits {
  
  implicit def treeToStream( tree : XmlTree ) : Iterator[PullType] = new TreeIterable(tree)
  
  implicit def dslToStream( ds : DslBuilder ) : Iterator[PullType] = new TreeIterable(ds.toTree)
  
}
