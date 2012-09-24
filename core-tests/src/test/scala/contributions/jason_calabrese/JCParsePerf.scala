package contributions.jason_calabrese

import java.io.StringReader

import com.google.caliper.Param
/*
object RunJC extends App {
  val jc = new JCParsingPerformance()
  jc.s = jc.allTheXml(1)
  println(jc.s)
  jc.timeScalesXmlTreeNameValue(1)
}*/


object Mutable {

  import scales.xml._, ScalesXml._, Functions._, impl.TreeProxies

  import scales.utils._

  import scalaz._
  import Scalaz._
  import scalaz.IterV._

  /**
   * Collects all data belonging to an element that matches
   * the list. <top><middle><ofInterest> content </ofInterest><ofInterest....
   * onQNames(List("top"l, "middle"l, "ofInterest"l))
   * would return an iteratee that returned every <ofInterest> content </ofInterest>
   * as a path (each parent node containing only one child node).
   */
  def onQNamesMutable(qnames: List[QName]): ResumableIter[PullType, QNamesMatch] = {

    /*
     * The pairs allow the depth of each element to be followed.  In particular this stops both descent and ascent problems in the
     * pushing and popping on the stack.  I.e. it covers the case where you have nested repeating QNames, both when you are looking for them
     * and when your are not.  Don't pop to early and don't incorrectly force a done.
     */

    lazy val starter = Cont(step(Nil, (qnames.head, 0), qnames.tail.map((_, 0)), new TreeProxies(), false))

    def step(before: List[(QName, Int)], focus: (QName, Int), toGo: List[(QName, Int)], proxies: TreeProxies, collecting: Boolean)(s: Input[PullType]): ResumableIter[PullType, QNamesMatch] =
      s(el = { e => //println(e +" "+before+" "+focus+" " + toGo); 
        e match {

          case Left(elem@Elem(q, a, n)) => {
            val nfocus = if (q == focus._1) (focus._1, focus._2 + 1)
            else focus
	    
	    proxies.beginSub(elem, XmlBuilder())
            //val npath = addAndFocus(path, elem)

            val shouldCollect = collecting || (toGo.isEmpty && q == focus._1)

            Cont(
              // is it our head?
              if ((!toGo.isEmpty) && q == focus._1)
                // move down
                step(before :+ focus, toGo.head, toGo.tail, proxies, false)
              else
                // wait for a down
                step(before, nfocus, toGo, proxies, shouldCollect))
          }

          case Left(x: XmlItem) =>
            if (collecting) {// collect 
	      //addChild(path, x)
	      proxies.addChild(x)
              Cont(step(before, focus, toGo, proxies, true))
	    }
            else
              Cont(step(before, focus, toGo, proxies, false)) // don't collect

          case Right(EndElem(q, n)) =>

            if (q == focus._1) {
              val ncfocus = (focus._1, focus._2 - 1)

              if (toGo.isEmpty && ncfocus._2 == 0) { // we are popping to the selected level
		val path = proxies.proxyPath
                Done(((qnames, Some(path)), // Some(path)
                  Cont(step(before, ncfocus, toGo,
                    // remove all children on the next iteration path.removeAndUp.getOrElse(noXmlPath)
                    proxies, false))), IterV.Empty[PullType])
	      }
              else {
                if (before.isEmpty)
                  starter // only when the root is asked for, could just refuse that of course?		
                else {
                  if (collecting) {
                    // we are collecting but we still have more than 0 repeated qnames deep
		    proxies.elementEnd() // path.zipUp
                    Cont(step(before, ncfocus, toGo, proxies, true))
		  }
                  else {
                    // we aren't collecting but we are moving up, we just have repeated names 
                    val nfocus = before.last
                    val nbefore = before.dropRight(1)
                    Cont(step(nbefore, nfocus, focus :: toGo, // removeand
                      proxies.proxyRemoveAndUp, false // we have NOT been collecting	
                      ))
                  }
                }
              }
            } else {
              Cont(step(before, focus, toGo,
                if (collecting) { // empty is not enough, it should also be definitely collecting
                  //path.zipUp
		  proxies.elementEnd
		  proxies
                } else
                  proxies.proxyRemoveAndUp, collecting))
            }

        }
      },
        empty = Cont(step(before, focus, toGo, proxies, false)),
        eof = Done(((qnames, None), starter), IterV.EOF[PullType]))

    if (qnames.isEmpty) error("Qnames is empty")

    starter
  }

  def iterateMutable(path: List[QName], xml: XmlPull): FlatMapIterator[XmlPath] = new IterateMutable(path, xml.it)

  class IterateMutable(path: List[QName], xml: Iterator[PullType]) extends FlatMapIterator[XmlPath] { self =>
    import ScalesUtils._

    val orig = withIter(xml)(onQNamesMutable(path))
										   

											def getNext = {
      if (orig.hasNext) {
        val t = orig.next
        if (t._2.isDefined)
          (true, t._2)
        else (false, None)
      } else (false, None)
    }
    var cur = getNext
    def hasNext = cur._1 && cur._2.isDefined
    def next = {
      val t = cur._2
      cur = getNext
      t.get
    }
        
  }
/* */
  def iterateMutable2(path: List[QName], xml: XmlPull): FlatMapIterator[XmlPath] = new IterateMutable2(path, xml.it)

  class IterateMutable2(qnames: List[QName], xml: Iterator[PullType]) extends FlatMapIterator[XmlPath] 
   { self =>
    import ScalesUtils._

    if (qnames.isEmpty) error("QNames is empty")
    
    /* see onQName for implementation basis */

    var before: List[(QName, Int)] = _
    var focus: (QName, Int) = _
    var toGo: List[(QName, Int)] = _
    var proxies: TreeProxies = new TreeProxies()
    var collecting: Boolean = _

    def reset {
      set(Nil, (qnames.head, 0), qnames.tail.map((_,0)), 
	  proxies.reuse, false)
    }

    reset

    def set(before: List[(QName, Int)], focus: (QName, Int), toGo: List[(QName, Int)], proxies: TreeProxies, collecting: Boolean) {
      this.before = before
      this.focus = focus
      this.toGo = toGo
      this.proxies = proxies
      this.collecting = collecting
    }

    def getNext = step
    var cur = getNext

    def hasNext = cur ne null
    def next = {
      val t = cur
      cur = getNext
      t
    }
    
    def step : XmlPath = {
      var res : XmlPath = null.asInstanceOf[XmlPath]
      while(xml.hasNext && res == null) { 
	val e = xml.next
        e match {

          case Left(elem@Elem(q, a, n)) => {
            val nfocus = 
	      if (q == focus._1) (focus._1, focus._2 + 1)
              else focus
	    
	    proxies.beginSub(elem, XmlBuilder())
            //val npath = addAndFocus(path, elem)

            val shouldCollect = collecting || (toGo.isEmpty && q == focus._1)

            // is it our head?
            if ((!toGo.isEmpty) && q == focus._1)
              // move down
              set(before :+ focus, toGo.head, toGo.tail, proxies, false)
            else
              // wait for a down
              set(before, nfocus, toGo, proxies, shouldCollect)
          }

          case Left(x: XmlItem) =>
            if (collecting) {// collect 
	      //addChild(path, x)
	      proxies.addChild(x)
              set(before, focus, toGo, proxies, true)
	    }
            else
              set(before, focus, toGo, proxies, false) // don't collect

          case Right(EndElem(q, n)) =>

            if (q == focus._1) {
              val ncfocus = (focus._1, focus._2 - 1)

              if (toGo.isEmpty && ncfocus._2 == 0) { // we are popping to the selected level
		res = proxies.proxyPath
		set(before, ncfocus, toGo,
                    // remove all children on the next iteration path.removeAndUp.getOrElse(noXmlPath)
                    proxies, false)
	      }
              else {
                if (before.isEmpty)
                  reset // only when the root is asked for, could just refuse that of course?		
                else {
                  if (collecting) {
                    // we are collecting but we still have more than 0 repeated qnames deep
		    proxies.elementEnd() // path.zipUp
                    set(before, ncfocus, toGo, proxies, true)
		  }
                  else {
                    // we aren't collecting but we are moving up, we just have repeated names 
                    val nfocus = before.last
                    val nbefore = before.dropRight(1)
                    set(nbefore, nfocus, focus :: toGo, // removeand
                      proxies.proxyRemoveAndUp(), false // we have NOT been collecting	
                      )
                  }
                }
              }
            } else {
              set(before, focus, toGo,
                if (collecting) { // empty is not enough, it should also be definitely collecting
                  //path.zipUp
		  proxies.elementEnd
		  proxies
                } else
                  proxies.proxyRemoveAndUp(), collecting)
            }

        }
      }

      res
    }

    
  }
  
}

/**
 * Timings for a "typical" recon style file
 */ 
class JCParsingPerformance extends scales.xml.SimpleScalaBenchmark {

  @Param(Array("5000","10000"))//"50","100","500","1000"))
  val size: Int = 0
 
  var s : String = _

  override def setUp() {
    s = allTheXml(size)
    //println("processing .." + s)
  }

  def visitEach(refs: Iterator[Map[String, List[String]]]) = {
    val r = refs.map(_.size).sum
    //print(r+",")
    if (r != (21 * size)) 
      scales.utils.error("was " +r)
    else
      r
  }
/*
  def timeScalesXmlTreeNameValue(reps: Int) = repeat(reps){
    import scales.xml._, ScalesXml._, Functions._, strategies.QNameTreeOptimisation
    import scales.utils._

    val xml = loadXml(new StringReader(s), strategy = strategies.QNameTreeOptimisation)
    
    visitEach(
      for {
	reference <- top(xml.rootElem).\*("reference"l).toIterator
      } yield {
	for {
	  child <- reference.\*
	} yield {
          (localName(child), text(child))
	}
      }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }

  def timeScalaXMLTree(reps: Int) = repeat(reps){
    import scala.xml.{Elem, XML}

    val xml = XML.load(new StringReader(s))
    
    visitEach(
    (xml \\ "reference").toIterator.map { ref =>
      ref.child.toList.collect {
        case child: Elem => (child.label, child.text)
      }.groupBy(_._1.toUpperCase).mapValues(_.map(_._2))
    }
    )
  }

  def timeScalesXmlTree(reps: Int) = repeat(reps){
    import scales.xml._, ScalesXml._, Functions._ 
    import scales.utils._

    val xml = loadXml(new StringReader(s))
    
    visitEach(
      for {
	reference <- top(xml.rootElem).\*("reference"l).toIterator
      } yield {
	for {
	  child <- reference.\*
	} yield {
          (localName(child), text(child))
	}
      }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
*/

  def timeScalesIterateMutable(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- Mutable.iterateMutable2(List("references"l, "reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- raw(reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }

  def timeScalesIterateMutableNonRaw(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- Mutable.iterateMutable2(List("references"l, "reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- (reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
/*
  def timeScalesIterateFull(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- iterate(List("references"l, "reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- (reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
*/
/*
  def timeScalesIterateFullOld(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    import scales.utils._

  def iterateOld(path: List[QName], xml: XmlPull): FlatMapIterator[XmlPath] = iterate(path, xml.it)

  class IterateOld(path: List[QName], xml: Iterator[PullType]) extends FlatMapIterator[XmlPath] { self =>
    import ScalesUtils._

    val orig = withIter(xml)(onDone(List(onQNames(path))))
    def getNext = {
      if (orig.hasNext) {
        val t = orig.next
        if (t.size == 1)
          (true, t.head._2)
        else (false, None)
      } else (false, None)
    }
    var cur = getNext
    def hasNext = cur._1 && cur._2.isDefined
    def next = {
      val t = cur._2
      cur = getNext
      t.get
    }
        
  }


    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- iterateOld(List("references"l, "reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- (reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }

/*
  def timeScalesIterateFullChildren(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
      for {
      reference <- iterate(List("references"l, "reference"l), pull)
    } yield {
      for {
//        children <- (reference \\)
//        child <- (children *)
	child <- reference \*
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
 
  def timeScalesIterate(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- iterate(List("reference"l), pull)
    } yield {
      for {
        //children <- (reference \\)
        //child <- (children *)
	child <- (reference \*)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
*/
*/
  def timePullMutable( reps : Int ) = repeat(reps){
    import scales.xml._, ScalesXml._
    import scala.collection.mutable.ListBuffer
// Iterator[Map[String, List[String]]] 
    var inRef = false
    var currentField: String = null
    val currentRef = new ListBuffer[(String, String)]
    val refs = new ListBuffer[List[(String, String)]]

    for (pull <- pullXml(new StringReader(s))) {
      pull match {
        case Left(event) => event match {
          case Elem(name, _, _) if name.local == "reference" =>
            inRef = true
          case Elem(name, _, _) if inRef =>
            currentField = name.local
          case text: Text if inRef && !text.value.trim.isEmpty =>
            currentRef.append((currentField, text.value))
          case other => //do nothing
        }
        case Right(end) if end.name.local == "reference" =>
          inRef = false
          refs.append(currentRef.toList)
          currentRef.clear
        case _ => //do nothing
      }
    }

    visitEach(
      refs.toIterator.map(_.groupBy(_._1.toUpperCase).mapValues(_.map(_._2)))
    )
  }
/**/
  
/*  def timeScalesIterateChildren(reps: Int) = repeat(reps) {
    import scales.xml._, ScalesXml._, Functions._

    val pull = pullXml(new StringReader(s))///streamToSource(stream))

    visitEach(
    for {
      reference <- iterate(List("reference"l), pull)
    } yield {
      for {
        children <- (reference \*)
        child <- (children *)
      } yield {
        (localName(child), text(child))
      }
    }.groupBy(_._1.toUpperCase).mapValues(_.toList.map(_._2))
    )
  }
  */
  def allTheXml(refCount: Int) : String = 
    <references>
    {
       1 to refCount map { id =>
                <reference>
                  <rt>Journal Article</rt>
                  <sr>Print(0)</sr>
                  <id>{ id }</id>
                  <a1>Amano,H.</a1>
                  <a1>Ogawa,H.</a1>
                  <a1>Maki,H.</a1>
                  <a1>Tsukamoto,S.</a1>
                  <a1>Yonezawa,Y.</a1>
                  <a1>Hahn,A. W.</a1>
                  <a1>Caldwell,W. M.</a1>
                  <t1>Relationship between frequency and impedance change in an infusion rate measurement system employing a capacitance sensor - biomed 2011</t1>
                  <jf>Biomedical sciences instrumentation</jf>
                  <jo>Biomed.Sci.Instrum.</jo>
                  <yr>2011</yr>
                  <vo>47</vo>
                  <sp>153</sp>
                  <op>159</op>
                  <ab>We have been searching for a suitable frequency range for an electrical impedance measurement infusion solution drip monitoring system, which we have previously reported. This electrical impedance, which is formed between two electrodes wrapped around the infusion supply polyvinyl-chloride tube and around the drip chamber, is changed by the growth and fall of each drop of fluid. Thus, the drip rate can be detected by measuring this impedance. However, many different kinds of infusion solutions such as glucose, amino acid, soya oil, and lactated Ringers solution are used in hospitals and care facilities. Therefore, it was necessary to find a suitable frequency for driving the capacitance-change sensor with a wide range of infusion solutions. In this study, the sensor electrical impedance change of 16 infusion solutions was measured from 1 kHz up to 1 MHz. The drip impedance produced by 5% glucose solution, 10% glucose solution and soya oil indicated the maximum sensor output change at 10 kHz, 20 kHz, and 70 kHz, respectively. The other 13 infusion solutions increased up to 10 kHz, and were constant from 10 kHz to 1 MHz. However, the growth, fall, and drip rate of the drops of all the infusion solutions were monitored by measuring the impedance change from 10 kHz to 30 kHz. Our experimental results indicated that most suitable excitation range for the infusion monitoring system is from 10 kHz to 30 kHz. Thus, we can now fine-tune the system for optimal sensing.</ab>
                  <no>JID: 0140524; ppublish</no>
                  <pp>United States</pp>
                  <sn>0067-8856; 0067-8856</sn>
                  <ad>Hiroshima Institute of Technology, Hiroshima, Japan.</ad>
                  <an>PMID: 21525613</an>
                  <la>eng</la>
                  <sf>Journal Article; IM</sf>
                  <ol>Unknown(0)</ol>
                  <pmid>21525613</pmid>
                </reference>
			    }			    
    } </references>.toString
}
