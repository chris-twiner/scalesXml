package scales.utils

/**
 * Very simple logger, matches scala more.  Each function returns true if the message would be logged.
 * 
 * Each function takes an optional second parameter of a throwable.
 * 
 * Implementations need only implement the log function.
 */
trait Logger {

  def trace(msg: => String, throwable: => Throwable): Boolean = log(Trace, msg, Some(throwable))
  def debug(msg: => String, throwable: => Throwable): Boolean = log(Debug, msg, Some(throwable))
  def info(msg: => String, throwable: => Throwable): Boolean = log(Info, msg, Some(throwable))
  def warn(msg: => String, throwable: => Throwable): Boolean = log(Warn, msg, Some(throwable))
  def error(msg: => String, throwable: => Throwable): Boolean = log(Error, msg, Some(throwable))
 
  def trace(msg: => String): Boolean = log(Trace, msg, None)
  def debug(msg: => String): Boolean = log(Debug, msg, None)
  def info(msg: => String): Boolean = log(Info, msg, None)
  def warn(msg: => String): Boolean = log(Warn, msg, None)
  def error(msg: => String): Boolean = log(Error, msg, None)

  /**
   * Special one, logs against some specific logging level.  Wouldn't recommend using it,
   * just use a special named logger instead.
   */
  def log(level: LogLevel, msg: => String, throwable: => Option[Throwable] = None): Boolean
}

/**
 * Simple trait for logging.
 */
trait Logs {
  protected[this] val log = Loggers.logger(this.getClass)
}

/**
 * Simple loglevel marker trait
 */
sealed trait LogLevel
case object Trace extends LogLevel
case object Debug extends LogLevel
case object Info extends LogLevel
case object Warn extends LogLevel
case object Error extends LogLevel

/**
 * Default implementation of a logger, for some reaason if its not
 * excluded (via private) or we change the type to no longer inherit
 * from logger we get a java.lang.UnsupportedOperationException: Position.Point
 * exception
 */
private[utils] class Slf4jLogger(logger : org.slf4j.Logger) extends Logger {
  def log(level: LogLevel, msg: => String, throwable: => Option[Throwable]): Boolean = {
      level match {
        case Trace => if (logger.isTraceEnabled()) {if (throwable.isDefined) logger.trace(msg, throwable.get) else logger.trace(msg); true} else false
        case Debug => if (logger.isDebugEnabled()) {if (throwable.isDefined) logger.debug(msg, throwable.get) else logger.debug(msg); true} else false
        case Info => if (logger.isInfoEnabled()) {if (throwable.isDefined) logger.info(msg, throwable.get) else logger.info(msg); true} else false
        case Warn => if (logger.isWarnEnabled()) {if (throwable.isDefined) logger.warn(msg, throwable.get) else logger.warn(msg); true} else false
        case Error => if (logger.isErrorEnabled()) {if (throwable.isDefined) logger.error(msg, throwable.get) else logger.error(msg); true} else false
      }
    }
}

/**
 * Contains the logic for getting a logger
 */
object Loggers {
  def logger[T](clazz: Class[T]) = new Slf4jLogger(org.slf4j.LoggerFactory.getLogger(clazz))
  def logger[T](category: String) = new Slf4jLogger(org.slf4j.LoggerFactory.getLogger(category))
}

