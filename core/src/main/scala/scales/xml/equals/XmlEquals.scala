package scales.xml.equals

import scales.xml._

import scalaz._
import Scalaz._

/**
 * Where did it fail
 */ 
object BasicPaths {
  // {ns}Local -> count
  type BasicPathA = (QName, Map[String, Int])
  type BasicPath = List[BasicPathA] 

  /**
   * Pushes a new elem on the stack, modifying the parents counts as it goes
   */ 
  def startElem( qname : QName, path : BasicPath ) : BasicPath =
    if (path.isEmpty) 
      scales.utils.one((qname, Map[String, Int]()))
    else {
      val (h :: r) = path
      val qn = qname.qualifiedName
      val count = h._2.get(qn).getOrElse(0)
      (qname, Map[String, Int]()) :: 
      (h._1, h._2.updated(qn, count + 1)) :: r
    }

  /**
   * Produces an XPath like string.  Make this a full one?
   */ 
  def pathString( path : BasicPath ) : String = {
    var str = ""
    var l = path//.reverse
    while( ! l.isEmpty ) {
      val head :: tail = l
      l = tail

      val qn = head._1.qualifiedName
      val count =
	if (tail.isEmpty) 1 
	else {
          val nhead :: rest = tail
	  nhead._2(qn)
	}
      
      str = "/"+ qn + "["+ count +"]" + str
    }
    str
  }

  /**
   * path.tail, pops the head off as it moves up, counts are not changed
   */ 
  def endElem( path : BasicPath ) : BasicPath = 
    path.tail
}

import BasicPaths._

/**
 * Why did equality fail, pattern match fun
 */ 
sealed trait XmlDifference[X] {
  val left : X
  val right : X
}

/**
 * Magik object for when we aren't attempting to calculate whats wrong 
 */ 
protected[xml] object SomeDifference extends XmlDifference[AnyRef] {
  val left = null
  val right = null

  def unapply(a : XmlDifference[AnyRef]) : Option[(AnyRef, AnyRef)] = Some((left,right))

  val noCalculation : Option[(XmlDifference[_], BasicPath)] = Some((this, Nil))
}

import SomeDifference.noCalculation

/**
 * When types are different, or an end element vs XmlEvent
 */ 
case class DifferentTypes( left : PullType, right : PullType ) extends XmlDifference[PullType]

sealed trait AttributeDifference extends XmlDifference[Attribute]
sealed trait ElemDifference extends XmlDifference[Elem]
sealed trait AttributesDifference extends XmlDifference[Attributes]

case class AttributeNameDifference( left : Attribute, right : Attribute ) extends AttributeDifference
case class AttributeValueDifference( left : Attribute, right : Attribute ) extends AttributeDifference

/**
 * Their size is different
 */ 
case class DifferentNumberOfAttributes( left : Attributes, right : Attributes ) extends AttributesDifference

/**
 * Re-packs the AttributeValueDifference, but will only occur when the attributes QName is present
 */
case class DifferentValueAttributes( left : Attributes, right : Attributes, differentValue : Attribute ) extends AttributesDifference

/**
 * The size is the same but one of lefts qnames is not present in right
 */ 
case class MissingAttributes( left : Attributes, right : Attributes, missing : Attribute ) extends AttributesDifference

case class ItemDifference( left : XmlItem, right : XmlItem) extends XmlDifference[XmlItem]

case class ElemNameDifference( left : Elem, right : Elem ) extends ElemDifference

/**
 * If the difference is due to an attribute thats included as well
 */ 
case class ElemAttributeDifference( left : Elem, right : Elem, attributesDifference : AttributesDifference ) extends ElemDifference

/**
 * This can only be returned when an ElemComparisom has been suplied with a custom namespaces implementation.
 */ 
case class ElemNamespacesDifference( left : Elem, right : Elem ) extends ElemDifference

case class EndElemNameDifference( left : EndElem, right : EndElem ) extends XmlDifference[EndElem]

/**
 * Like Equals but also gives a path in addition to the fun reason
 * 
 */ 
trait XmlComparison[T] {
  /**
   * Takes the path for information reasons (works for streams as well).  The return is either None == boolean or the reason
   */ 
  def compare( calculate : Boolean , path : BasicPath, left : T, right : T) : Option[(XmlDifference[_], BasicPath)]
}

object ItemEquals {

  implicit object DefaultXmlItemComparison extends XmlComparison[XmlItem] {
    def compare( calculate : Boolean , path : BasicPath, left : XmlItem, right : XmlItem) : Option[(XmlDifference[_], BasicPath)] = {
      def check( str : String, str2 : String ) =
	  if (str == str2) None
	  else {
	    if (calculate) 
	      Some((ItemDifference(left, right), path))
	    else
	      noCalculation
	  }

      (left, right) match { // we have to do it on types as well
	case (Text(valu), Text(value)) => check(valu, value)
	case (Comment(com), Comment(comm)) => check(com, comm)
	case (CData(cd), CData(cda)) => check(cd, cda)
	case (PI(ta, valu), PI(tar, value)) => 
	  check(ta, tar) orElse check(valu,  value)
	case _ => 
	  if (calculate)
	    Some((DifferentTypes(left, right), path))
	  else
	    noCalculation
      }
    }
  }
  
  implicit val defaultXmlItem : Equal[XmlItem] = equal {
    (a : XmlItem, b : XmlItem) =>
      DefaultXmlItemComparison.compare(false, Nil, a, b).isEmpty 
  }
}

object AttributeEquals {
  class AttributeComparison(implicit eqn : Equal[QName]) extends XmlComparison[Attribute] {
    import ScalesXml.toQName

    def compare( calculate : Boolean, path : BasicPath, left: Attribute, right : Attribute) : Option[(XmlDifference[_], BasicPath)] = {
      if (!eqn.equal(toQName(left.name), toQName(right.name)))
	if (calculate)
	  Some((AttributeNameDifference( left, right), path))
	else
	  noCalculation
      else 
	if (left.value != right.value)
	  if (calculate)
	    Some((AttributeValueDifference( left, right), path))
	  else
	    noCalculation
	else
	  None // a ok
    }
  }

  /**
   * QNames are not compared with prefix
   */ 
  implicit val defaultAttributeComparison = new AttributeComparison()(ScalesXml.qnameEqual)
  
  /**
   * QNames are not compared with prefix
   */ 
  implicit val defaultAttributeEquals = equal { 
    (a : Attribute, b : Attribute) =>
      defaultAttributeComparison.compare(false, Nil, a, b).isEmpty
  }

  /**
   * Comparisoms built on exact qname comparisom, prefixes can be important for some systems
   */ 
  object ExactQName {
    /**
     * QNames are compared with prefix
     */ 
    implicit val prefixAttributeComparison = new AttributeComparison()(equal { (a: QName, b: QName) => a.====(b) })
    
    /**
     * QNames are compared with prefix
     */ 
    implicit val prefixAttributeEquals = equal { 
      (a : Attribute, b : Attribute) =>
	prefixAttributeComparison.compare(false, Nil, a, b).isEmpty
    }
  }
}

object AttributesEquals {

  class AttributesComparison( implicit ac : XmlComparison[Attribute]) extends XmlComparison[Attributes] {
    def compare( calculate : Boolean, path : BasicPath, left : Attributes, right : Attributes) : Option[(XmlDifference[_], BasicPath)] = {
      import EqualsHelpers._
      import scales.utils.collectFirst

      if (left.size != right.size)
	if (calculate)
	  Some((DifferentNumberOfAttributes(left,right), path))
	else
	  noCalculation
      else 
	// get the first error
	collectFirst[Attribute, (XmlDifference[_], BasicPath)](left){ 
	  x1 =>
	    right( x1.name ).cata( x2 => {// if we have it check the attributes
		val r = ac.compare(calculate, path, x1, x2)
		if (r.isDefined) {
		  // it can only be AttributeValueDifference
		  if (calculate)
		   Some( (DifferentValueAttributes( left, right, x1 ), path) )
		  else
		    noCalculation
		} else None
	      } , 
	      if (calculate)
		Some( ( MissingAttributes( left, right, x1 ), path) )
	      else
		noCalculation     
	    )
	}
    }
  }

  implicit val defaultAttributesComparison = new AttributesComparison()( AttributeEquals.defaultAttributeComparison )

  implicit val defaultAttributesEquals = equal {
    (a : Attributes, b : Attributes) =>
      defaultAttributesComparison.compare(false, Nil, a, b).isEmpty
  }
}

object ElemEquals {

  type NamespacesEqual = Equal[Map[String, String]]
  val allwaysTrueNamespacesEqual = equal {
    ( a : Map[String, String], b : Map[String, String] ) => true
  }

  /**
   * Namespaces by default are not compared.  They aren't marked as implicit as you really have to be explicitly wanting to compare them.
   */ 
  class ElemComparison(namespaces : Equal[Map[String, String]] = allwaysTrueNamespacesEqual)( implicit ac : XmlComparison[Attributes], eqn : Equal[QName]) extends XmlComparison[Elem] {
    def compare( calculate : Boolean, path : BasicPath, left : Elem, right : Elem) : Option[(XmlDifference[_], BasicPath)] = {

      if (!(left.name === right.name))
	if (calculate)
	  Some((ElemNameDifference(left,right), path))
	else
	  noCalculation
      else 
	ac.compare(calculate, path, left.attributes, right.attributes).
	  cata( x => 
	    if (calculate) {
	      val (attrs : AttributesDifference, p) = x
	      Some( (ElemAttributeDifference( left, right, attrs), path) )
	    } else noCalculation
	  ,
	    if (!namespaces.equal(left.namespaces, right.namespaces))
	      if (calculate)
		Some( (ElemNamespacesDifference(left, right), path) )
	      else
		noCalculation
	    else None
	  )
    }
  }

  implicit val defaultElemComparison = new ElemComparison()( AttributesEquals.defaultAttributesComparison, ScalesXml.qnameEqual)

  implicit val defaultElemEquals = equal {
    (a : Elem, b : Elem) =>
      defaultElemComparison.compare(false, Nil, a, b).isEmpty
  }
}

object StreamEquals {

  /**
   * Compares based on streams
   */ 
  class StreamComparison( implicit ic : XmlComparison[XmlItem], ec : XmlComparison[Elem], endElemQNameEqual : Equal[QName]) extends XmlComparison[Iterator[PullType]] {
    def compare( calculate : Boolean, opath : BasicPath, lefti : Iterator[PullType], righti : Iterator[PullType] ) : Option[(XmlDifference[_], BasicPath)] = {

      var res : Option[(XmlDifference[_], BasicPath)] = None
      val joined = lefti.zip(righti)
      var path = opath

      while( res.isEmpty && joined.hasNext) {
	joined.next match {
	  case (Left(x : Elem), Left(y : Elem)) =>
	    path = startElem(x.name, path)
	    res = ec.compare(calculate, path, x, y)
	  case (Left(x : XmlItem), Left(y : XmlItem)) =>
	    res = ic.compare(calculate, path, x, y)
	  case (Right(x : EndElem), Right(y  : EndElem)) =>
	    res = 
	      if (!endElemQNameEqual.equal(x.name, y.name)) {
		if (calculate)
		  Some((EndElemNameDifference(x, y), path))
		else 
		  noCalculation
	      } else None
	    path = endElem(path)
	  case (left, right) => 
	    res = 
	      if (calculate)
		Some((DifferentTypes(left, right), path)) 
	      else
		noCalculation
	}
      }

      res
    }
  }

  implicit val defaultStreamComparison : XmlComparison[Iterator[PullType]] = new StreamComparison()( ItemEquals.DefaultXmlItemComparison, ElemEquals.defaultElemComparison, ScalesXml.qnameEqual)

  implicit val defaultStreamEquals = equal {
    (a : Iterator[PullType], b : Iterator[PullType]) =>
      defaultStreamComparison.compare(false, Nil, a, b).isEmpty
  }
}

