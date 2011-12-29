package scales.utils

import ScalesUtils._

class LoggerTest extends junit.framework.TestCase {
  
  import junit.framework.Assert._
  
  class Dummy extends Logs {
    def trace : Unit = log.trace("trace")
    def debug : Unit = log.debug("debug")
    def info : Unit = log.info("info")
    def warn : Unit = log.warn("warn")
    def error : Unit = log.error("error")
    
    def oops : String = {fail("Should not have been called"); ""}
    def oopsT : Throwable = {fail("Should not have been called"); new Exception("")}
    def traceEval = log.trace(oops,oopsT)
    def debugEval = log.debug(oops,oopsT)
  }

  def testLogs = {
    val file = "./logs/testLog.log"
    new java.io.File(file).delete()
    val log = new Dummy
    log.trace
    log.debug
    log.info
    log.warn
    log.error

    log.traceEval
    log.debugEval

    val reader = new java.io.BufferedReader( new java.io.FileReader(file) )
    try { 
      val res = reader.readLine()
      assertEquals("INFOinfoWARNwarnERRORerror", res)
    } finally {
      reader.close
    }
  }
}
