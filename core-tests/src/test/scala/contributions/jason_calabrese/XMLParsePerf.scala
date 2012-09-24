package contributions.jason_calabrese

import java.io.FileInputStream
import java.io.File
import java.io.InputStream
import org.apache.commons.io.FileUtils._
import java.io.PrintWriter

object XMLParsePerf {

  def apply(args: String*): Unit = {

    val cmd = args.headOption
    val file = args.drop(1).headOption
    val size = args.drop(2).headOption.map(_.toInt)

    cmd match {
      case Some("perf") => (file, size) match {
        case (Some(filePath), Some(refCount)) =>

          def visitEach(refs: Iterator[Map[String, List[String]]]) = refs.foreach(_.size)

          System.gc
          val pullStart = System.currentTimeMillis
          val pullStartMem = Runtime.getRuntime.freeMemory
          val refsPull = XMLParse.parseXMLPullDSL(new FileInputStream(filePath))
          visitEach(refsPull)
          val pullTime = System.currentTimeMillis - pullStart
          val pullMem = pullStartMem - Runtime.getRuntime.freeMemory

          System.gc
          val pullMutableStart = System.currentTimeMillis
          val pullMutableStartMem = Runtime.getRuntime.freeMemory
          val refsPullMutable = XMLParse.parseXMLPullMutable(new FileInputStream(filePath))
          visitEach(refsPullMutable)
          val pullMutableTime = System.currentTimeMillis - pullMutableStart
          val pullMutableMem = pullMutableStartMem - Runtime.getRuntime.freeMemory

          System.gc
          val treeStart = System.currentTimeMillis
          val treeStartMem = Runtime.getRuntime.freeMemory
          val treeRefs = XMLParse.parseXMLTree(new FileInputStream(filePath))
          visitEach(treeRefs)
          val treeTime = System.currentTimeMillis - treeStart
          val treeMem = pullMutableStartMem - Runtime.getRuntime.freeMemory

          println("REF COUNT: " + refCount)
          println("FILE SIZE: " + byteCountToDisplaySize(sizeOf(new File(filePath))))

          def report(method: String, time: Long, mem: Long) =
            println("%s: time=%sms mem=%s".format(method, time, byteCountToDisplaySize(mem)))

          report("PULL (iterate/dsl)", pullTime, pullMem)
          report("PULL (mutable)", pullMutableTime, pullMutableMem)
          report("TREE (standard scala.xml)", treeTime, treeMem)

        case _ => usage
      }
      case Some("genfile") => (file, size) match {
        case (Some(filePath), Some(refCount)) =>
          printToFile(filePath) { p =>
            p.println("<references>")
            1 to refCount foreach { id =>
              p.println(
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
              )
            }
            p.println("</references>")
          }
        case _ => usage
      }
      case _ => usage
    }
  }

  def printToFile(path: String)(op: PrintWriter => Unit): Unit = {
    val p = new PrintWriter(new File(path))
    try { op(p) } finally { p.close() }
  }
  
  def usage = {
    println("""
    XMLParsePerf(command, file, size)
    EXAMPLES
      XMLParsePerf("genfile", "/Users/jcalabre/temp/ref_perf500.xml", "500")
      XMLParsePerf("perf", "/Users/jcalabre/temp/ref_perf500.xml", "500")

    PARAMETERS
      command  The command to execute [ genfile | perf ]
      file     path to the xml file used to gen or test with
      size     the # of ref to be added to the test xml file or reported in perf stats
    """)
  }
}
