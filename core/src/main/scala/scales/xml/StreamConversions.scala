package scales.xml

import scales.utils._

/**
 * All safe conversions from T to Iterator[PullType].
 *
 */ 
trait PullTypeConversionImplicits {
  
  implicit def treeToStream( tree : XmlTree ) : Iterator[PullType] = new trax.TreeIterable(tree)
  
  implicit def dslToStream( ds : DslBuilder ) : Iterator[PullType] = new trax.TreeIterable(ds.toTree)
  
}
