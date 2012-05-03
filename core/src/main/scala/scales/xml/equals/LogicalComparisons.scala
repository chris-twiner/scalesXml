package scales.xml.equals

import scales.xml.{PullType, Text, CData, PI, Comment}

/**
 * Modifies the stream to turn CData nodes into Text nodes and join all adjacent Text nodes togeter i.e. a "tree" with children (Text, CData, Text, CData) will become a single Text child.
 *
 * CData can be kept if keepCData is set to true
 */ 
class JoinTextAndCData( var it : Iterator[PullType], val keepCData : Boolean = false) extends Iterator[PullType] {

  var gotNext = false
  var item : PullType = _
  var nextIsNonText = false
  var nonText : PullType = _
  var sb : java.lang.StringBuilder = _

  def hasNext = gotNext

  def next = {
    if (!gotNext) throw new java.util.NoSuchElementException("No more data to give")
    var i = item
    pumpNext // set the next one up
    i
  }

  def pumpNext {
    if (nextIsNonText) {
      nextIsNonText = false
      // set gotNext to true as we have it anyway
      item = nonText
      gotNext = true
      return
    }
    // otherwise we must try again
    
    gotNext = false
    var inText = false

    def addText( str : String ) {
      if (!inText) {
	inText = true
	sb = new java.lang.StringBuilder()
      }
      sb.append(str)
    }

    while( !gotNext && it.hasNext) {
      item = it.next
      item match {
	case Left(x : Text) => addText(x.value)
	case Left(x : CData) if (!keepCData) => addText(x.value)
	case x : PullType => 
	  if (inText) {
	    nextIsNonText = true
	    nonText = item
	    // join them up
	    item = Left(Text(sb.toString))
	  } else {
	    nextIsNonText = false
	  }
	gotNext = true
      }
    }
  }

  pumpNext // start it off
}


/**
 * A collection of stream filters to help with equality
 */ 
object LogicalFilters {
  
  /**
   * Modifies the stream to turn CData nodes into Text nodes and join all adjacent Text nodes togeter i.e. a "tree" with children (Text, CData, Text, CData) will become a single Text child. 
   */ 
  def joinTextAndCData( it : Iterator[PullType] ) : Iterator[PullType] = 
    new JoinTextAndCData(it)

  /**
   * Joins all adjacent Text nodes together but keeps CData nodes
   * @see joinTextAndCData
   */ 
  def joinText( it : Iterator[PullType] ) : Iterator[PullType] =
    new JoinTextAndCData(it, true)

  /**
   * Removes all comments and PIs.  This is not used by default.
   */ 
  def removePIAndComments( it : Iterator[PullType] ) : Iterator[PullType] =
    it.filter{
      case Left(_ : PI) | Left(_ : Comment) => false
      case _ => true
    }
}
