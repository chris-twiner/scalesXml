package scales.xml

import scales.utils._
import ScalesUtils._
import scales.xml._
import ScalesXml._

import scalaz._
import Scalaz._

import java.io.StringReader
import javax.xml.transform._
import dom._
import stream._

import strategies._

object QNameAndAttr extends PathOptimisationStrategy[ElemToken] with ElemQNameOptimisationT[ElemToken] with ElemTokenF {

  // register the qname for eq style
  val tyype = {
    val t = createToken
    noNamespaceQName("type", t)
  }

  val primary = Attribute(tyype, "primary")
  val secondary = Attribute(tyype, "secondary")  

  override def attribute( qname : AttributeQName, value : String, token : ElemToken) : Attribute = 
    if (qname eq tyype)
      if (value == "primary")
	primary
      else
	secondary
    else
      Attribute(qname,value)
  
}

object PerfData {
  val ns = Namespace("uri:reconstyle")

  def itemVersion( i : Int ) : EphemeralStream[PullType] =
    EphemeralStream[PullType](
      Left(Elem(ns("id"))),
      Left(Text(i.toString)),
      Right(EndElem(ns("id"))),
      Left(Elem(ns("version"))),
      Left(Text(i.toString)),
      Right(EndElem(ns("version")))
    )

  def wrapVersion( i : Int, max : Int, idtype : String ) : EphemeralStream[PullType] = 
    if (i < max) {
      EphemeralStream[PullType](
	Left(Elem(ns(idtype)))
      ) ++ itemVersion(i) +:+ 
      EphemeralStream[PullType](
	Right(EndElem(ns(idtype)))
      ) ++ wrapVersion(i + 1, max, idtype)
    } else EphemeralStream[PullType]()

  def wrapVersions( i : Int, max : Int, idtype : String ) : EphemeralStream[PullType] = {
    EphemeralStream[PullType]( Left(Elem(ns(idtype + "s"))) ) ++
    wrapVersion(i, max, idtype) ++
    EphemeralStream[PullType]( Right(EndElem(ns(idtype + "s"))) )
  }

  def reconDoc( max : Int ) : EphemeralStream[PullType] = 
    {
      EphemeralStream[PullType]( Left(Elem(ns("Recon"))) ) ++
      wrapVersions(0, max, "Part") ++
      wrapVersions(0, max, "BOM") ++
      wrapVersions(0, max, "Record") ++
      EphemeralStream[PullType]( Right(EndElem(ns("Recon"))) )
    }

}

import com.google.caliper.Param

class SpecificFilePerformance extends SimpleScalaBenchmark {

  @Param
  val fname : String = ""

  var s : String = _

  override def setUp() {
    val sor = scala.io.Source.fromFile(fname,"UTF-8")
    s = sor.mkString
    sor.close
  }

  def loadXmlS[T <: OptimisationToken]( s : String, ostrategy : PathOptimisationStrategy[T]) = 
    loadXml[T](new java.io.StringReader(s), strategy = ostrategy)

 // def loadXmlP( s : String, ostrategy : PathOptimisationStrategy) = 
   // pullXmlCompletely(new java.io.StringReader(s), strategy = ostrategy)
  def timeScala(reps: Int) = repeat(reps) {
    scala.xml.XML.loadString(s)
  }  

  def timeQNameOnly(reps: Int) = repeat(reps) {
    loadXmlS(s, QNameMemoryOptimisation)
  }

  def timeTyped(reps: Int) = repeat(reps) {
    loadXmlS(s, QNameAndAttr)
  }

}

object Recon {
  import PerfData.ns

  val ReconQ = ns("Recon")
  val PartsQ = ns("Parts")
  val PartQ = ns("Part")
  val BomsQ = ns("BOMs")
  val BomQ = ns("BOM")
  val RecsQ = ns("Records")
  val RecQ = ns("Record")

  val Part = List(ReconQ, PartsQ, PartQ)
  val Bom = List(ReconQ, BomsQ, BomQ)
  val Rec = List(ReconQ, RecsQ, RecQ)

  val id = ns("id")
  val version = ns("version")

  val versionf = {
    import Elements.Functions.text
//    (x : XmlPath) => (text(raw(x.\*(id)).head).toInt, text(raw(x.\*(version)).head).toInt)
    (x : XmlPath) => (text(x.\*(id).head).toInt, text(x.\*(version).head).toInt)
  }

}

case class Recon(val parts : Map[Int, Int] = Map[Int, Int](),
		 val boms : Map[Int, Int] = Map[Int, Int](),
		 val recs : Map[Int, Int] = Map[Int, Int]())


/**
 * Timings for a "typical" recon style file
 */ 
class ParsingPerformanceRecon extends SimpleScalaBenchmark {

  @Param(Array("10000", "40000"))//"10", "100", "1000"))//, "10000", "40000"))//
  val size: Int = 0
  
  var s : String = _

  override def setUp() {
    s = asString(PerfData.reconDoc(size).iterator)
  }
  
  def loadXmlS[T <: OptimisationToken]( s : String, ostrategy : PathOptimisationStrategy[T]) = 
    loadXml[T](new java.io.StringReader(s), strategy = ostrategy)

  def loadXmlP[T <: OptimisationToken]( s : String, ostrategy : PathOptimisationStrategy[T]) = 
    pullXmlCompletely[T](new java.io.StringReader(s), strategy = ostrategy)
/*  
  def timeNoOptimisation(reps: Int) = repeat(reps) {
    loadXmlS(s, NoOptimisation)
  } */

/*
 * 

  def timeScalaXML(reps: Int) = repeat(reps) {
    scala.xml.XML.loadString(s)
  }
  def timeScalesXmlTreeOp(reps: Int) = repeat(reps) {
    loadXmlS(s, QNameTreeOptimisation)
  }

*/


  def timeScalesXml(reps: Int) = repeat(reps) {
    loadXmlS(s, QNameMemoryOptimisation)
  }

  /**
   * How long for pull based, low memory onqnames action
   * 
  def timePullCollect(reps: Int) = repeat(reps) {
    import Recon._

    val xml = pullXml(new java.io.StringReader(s)).it
    foldOnDone( xml )( Recon() , 
		      onDone( List(onQNames(Part), 
				   onQNames(Bom), 
				   onQNames(Rec)) )) {
      (recon, qNamesMatch) => 
	if (qNamesMatch.size == 0)
	  recon
	else {
	  // we expect only one to match in this pattern	  
	  val matched = qNamesMatch.head
	  val qnames = matched._1  // to get an onDone it must be defined
	  val x = matched._2.get
	  // only one child
	  val pair = versionf(x)
	  
	  //recon
	  qnames match {
	    case Part => recon.copy( parts = recon.parts + pair )
	    case Bom => recon.copy( boms = recon.boms + pair )
	    case Rec => recon.copy( recs = recon.recs + pair )
	  }/**/
	}			   
    
    }
  }

  def processFullParseCollect = {
    import Recon._

    val doc = loadXmlS(s, QNameAndSpeedierStrategy)

    val root = top(doc.rootElem)//.\*(ReconQ)
    val parts = root.\*(PartsQ).\*(PartQ)
    val boms = root.\*(BomsQ).\*(BomQ)
    val recs = root.\*(RecsQ).\*(RecQ)
    
    (parts, boms, recs)
  }
*/
  /**
   * How long does it take for a full parse and collect?
  def timeFullParseCollect( reps: Int) = repeat(reps) {
    import Recon._

    val (parts, boms, recs) = processFullParseCollect

    Recon((parts map versionf).toMap,
	(boms map versionf).toMap,
	(recs map versionf).toMap)
  }
   */ 

  /**
   * How long does it take for a full parse and collect?
  def timeFullParseCollectRaw( reps: Int) = repeat(reps) {
    import Recon._

    val (parts, boms, recs) = processFullParseCollect

    Recon((raw(parts) map versionf).toMap,
	(raw(boms) map versionf).toMap,
	(raw(recs) map versionf).toMap)
  }
   */ 


  /**
   * Full parse and collect lazily with lazy evaluation of results
   * Doesn't even properly due to taking way to long to run for 40,000
   * 
  def timeFullParseDLRV( reps: Int) = repeat(reps) {
    import Elements.Functions.text

    val doc = loadXmlS(s, QNameAndSpeedierStrategy)

    val root = viewed(top(doc.rootElem))//.\*(ReconQ)
    
    // get all versions, sum em, silly but enough to see how deep works with and without views
    val all = lazyRaw(root.\\.*(Recon.version))
    all.foldLeft(0L){ (x,y) => text(y).toLong + x }.toString
  }*/

  /**
   * Test deep and filter, to evaluate differences in flatMap vs whileloop
   
  def timeFullParseDF( reps: Int) = repeat(reps) {
    import Elements.Functions.text
    
    import Recon._

    val doc = loadXmlS(s, QNameAndSpeedierStrategy)

    val root = top(doc.rootElem)
    val parts = root.\\.*(PartQ)
    val boms = root.\\.*(BomQ)
    val recs = root.\\.*(RecQ) // gets progressively worse for deep as it must renavigate from the top again
    
    val res = recs.foldLeft(0L){ (x,y) => text(y).toLong + x }.toString
    // 1000 is 495540450
    // 10000 is 495500035950
    //if (res != "495500035950") sys.error("Was not the expected result but "+res)
    res
  }*/

  /**
   * Full parse and collect lazily but evaluate via raw
   
  def timeFullParseDRV( reps: Int) = repeat(reps) {
    import Elements.Functions.text

    val doc = loadXmlS(s, QNameAndSpeedierStrategy)

    val root = viewed(top(doc.rootElem))//.\*(ReconQ)
    
    // get all versions, sum em, silly but enough to see how deep works with and without views
    val all = raw(root.\\.*(Recon.version))
    all.foldLeft(0L){ (x,y) => text(y).toLong + x }.toString
  }
*/
  /**
   * Full parse and collect eagerly (default behaviour)
  
  def timeFullParseDRE( reps: Int) = repeat(reps) {
    import Elements.Functions.text

    val doc = loadXmlS(s, QNameAndSpeedierStrategy)

    val root = top(doc.rootElem)//.\*(ReconQ)
    
    // get all versions, sum em, silly but enough to see how deep works with and without views
    val all = raw(root.\\.*(Recon.version))
    all.foldLeft(0L){ (x,y) => text(y).toLong + x }.toString
  }
 */
/*
  def timeMutableVectorLike(reps: Int) = repeat(reps) {
    object HighPerformanceStrategy extends MutableVectorLikeStrategy[BaseToken] with BaseTokenF
    loadXmlS(s, HighPerformanceStrategy)
  }



  def timeQNameAndSpeedPull(reps: Int) = repeat(reps) {
    loadXmlP(s, QNameAndSpeedierStrategy)
  }

  def timeJAXPParseDeferred(reps: Int) = repeat(reps) {
    DefaultDOMFactoryPool.loan{ x => 
      x.setFeature("http://apache.org/xml/features/dom/defer-node-expansion",
                    true);
      x.newDocumentBuilder.parse(new StringReader(s)) }
  }

  def timeJAXPParseFully(reps: Int) = repeat(reps) {
    DefaultDOMFactoryPool.loan{ x => 
      x.setFeature("http://apache.org/xml/features/dom/defer-node-expansion",
                    false);
      x.newDocumentBuilder.parse(new StringReader(s)) }
  }
  */


  
}

trait RunTest {
  def waitForTouch {
    val f = new java.io.File("./continue.txt")
    f.createNewFile
    val start = f.lastModified
    var touched = false
    while(!touched) {
      if (f.lastModified > start) {
	touched = true
      } else {
	Thread.sleep(10 * 1000)
      }
    }
  }

  def doTest : Unit

  def prepare(args: Array[String]) : Unit

  def main(args: Array[String]) {
    prepare(args)

    println("Start profiling now - update continue.txt when ready to start the run...")
    waitForTouch
    println("Starting Run.....")
    //p.timeHighPerformance(5)
    doTest
    println("Run over, get snapshot - update continue.txt to end")
    waitForTouch    
  }
}

trait SpecificFileTest extends RunTest {
  
  val p = new SpecificFilePerformance()

  def prepare(args: Array[String]) {
    val sor = scala.io.Source.fromFile(args(0),"UTF-8")
    p.s = sor.mkString
    sor.close
  }
}

trait ReconTest extends RunTest {
  
  val p = new ParsingPerformanceRecon()

  val size = 40000

  def prepare(args: Array[String]) {
    p.s = asString(PerfData.reconDoc(size).iterator)
  }
}

object RunHighPerf extends ReconTest {

//  override val size = 43000
  override val size = 5000

  var doc : DocLike = _

  def doTest {
    doc = p.timeScalesXml(5)
//    doc = p.timeScalesXml(100)
    //doc = p.timeScalesXmlTreeOp(5)
  }
}
/*
object RunMemoryOptimised extends ReconTest {

  def doTest {
    p.timeMemoryAndSpeed(5)
  }
}

object RunDeferred extends ReconTest {

  override val size = 60000

  def doTest {
    p.timeJAXPParseDeferred(5)
  }
}

object RunNonDeferred extends ReconTest {

  def doTest {
    p.timeJAXPParseFully(5)
  }
}
*/
object RunPullCollect extends ReconTest {

  var res : Recon = _

  override val size = 40000

  def doTest {
    //res = p.timePullCollect(5)
  }
}

object RunParseCollect extends ReconTest {

  var res : Recon = _

  override val size = 40000

  def doTest {
//    res = p.timeFullParseCollect(5)
  }
}

object RunParseCollectRaw extends ReconTest {

  var res : Recon = _

  override val size = 40000

  def doTest {
//    res = p.timeFullParseCollectRaw(1)
  }
}

object RunParseDeepRawEager extends ReconTest {

  var res : Int = _

  override val size = 40000

  def doTest {
//    res = p.timeFullParseDRE(1)
  }
}

object RunMemoryOptimisedFile extends SpecificFileTest {

  def doTest {
    //p.timeQNameAndSpeed(2)
  }
}
