package org.jaxen.expr

import org.jaxen._
import pattern.Pattern
import expr._

import java.util.{Iterator => JIterator, ArrayList, List => JList, HashSet, Collections}

import scales.utils._
import ScalesUtils._
import scales.xml._
import ScalesXml._
import scales.xml.jaxen.ScalesComparator

class ScalesDefaultLocationPath() extends DefaultLocationPath {

  @throws(classOf[JaxenException])
  override def evaluate( context : Context ) : AnyRef= {
    val nodeSet = context.getNodeSet()
    var contextNodeSet : JList[AnyRef] = new ArrayList[AnyRef](nodeSet.asInstanceOf[ java.util.Collection[AnyRef]] )
    val support = context.getContextSupport
    val stepContext = new Context(support)
    val stepIter = getSteps().iterator()
    while(stepIter.hasNext){
      val eachStep = stepIter.next().asInstanceOf[Step]
      stepContext.setNodeSet(contextNodeSet);
      contextNodeSet = eachStep.evaluate(stepContext).asInstanceOf[ JList[AnyRef]];
      // now we need to reverse the list if this is a reverse axis
      if (isReverseAxis(eachStep)) {
        Collections.reverse(contextNodeSet);
      }
    }
    
    //if (getSteps().size() > 1 || nodeSet.size() > 1) {
    //  Collections.sort(contextNodeSet, ScalesComparator);
    //}
  
    contextNodeSet
  }
  
  /**
   * Was private
   */ 
  def isReverseAxis(step : Step) = {
    val axis =step.getAxis()
    axis == org.jaxen.saxpath.Axis.PRECEDING ||
     axis == org.jaxen.saxpath.Axis.PRECEDING_SIBLING ||
     axis == org.jaxen.saxpath.Axis.ANCESTOR ||
     axis == org.jaxen.saxpath.Axis.ANCESTOR_OR_SELF;
  }
}

/**
 * To use a proper comparator
 */ 
class ScalesUnionExpr(lhs : Expr, rhs : Expr) extends DefaultBinaryExpr(lhs,rhs) with UnionExpr {
  def getOperator() = "|"
  override def toString() = "[(ScalesUnionExpr): " + getLHS() + ", " + getRHS() + "]"

  def evaluate( context : Context) = {
    val results = new ArrayList[AnyRef]()
    try{
      val lhsResults = getLHS().evaluate( context ).asInstanceOf[JList[AnyRef]]
      val rhsResults = getRHS().evaluate( context ).asInstanceOf[JList[AnyRef]]
    
      results.addAll( lhsResults )

      val unique = new HashSet[AnyRef]()
      unique.addAll( lhsResults )
    
      var rhsIter = rhsResults.iterator
      while ( rhsIter.hasNext ) {
	val each = rhsIter.next
	if (!unique.contains(each)) {
	  results.add(each)
	  unique.add(each)
	}
      }
      
      //Collections.sort(results, ScalesComparator);
      results
    }
    catch {
      case e : ClassCastException =>
	e.printStackTrace
        throw new XPathSyntaxException(this.getText(), context.getPosition(), "Unions are only allowed over node-sets "+e);
    }
  }

}
