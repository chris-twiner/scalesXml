package scales.utils

import java.util.{ Locale, ResourceBundle, Calendar, Date }

/**
 * Based on the approach taken by Muse and others giving paramaterized messages using XXX as a token to replace.
 */
case class Resource(locale: () => Locale = () => Locale.getDefault)(file: (Class[_], Option[String])) {
  protected val PLACE_HOLDER = "XXX"

  protected val classLoader = file._1.getClassLoader
  protected val resourceName = if (file._2.isEmpty) file._1.getName
  else file._1.getPackage.getName + "." + file._2.get

  //
  // Used for requests that don't have any placeholders
  //
  protected val _EMPTY = new Array[Any](0)

  // TODO figure out how / if these should handle updated language packs at runtime??
  protected val localeToBundle = new java.util.concurrent.ConcurrentHashMap[Locale, ResourceBundle]

  protected def getResourceBundle() = {
    val ilocale = locale()
    //val ilocale = if (tlocale == null) Locale.getDefault else tlocale
    val bundle = localeToBundle.get(ilocale)
    if (bundle eq null) {
      val nbundle = ResourceBundle.getBundle(resourceName, ilocale, classLoader)
      val res = localeToBundle.putIfAbsent(ilocale, nbundle)
      if (res eq null) nbundle else res
    } else bundle
  }

  def apply(id: String): String = iget(id)(false)

  def apply(id: String, fillers: Any*): String = iget(id, fillers: _*)(false)

  def apply(showId: Boolean, id: String, fillers: Any*): String = iget(id, fillers: _*)(showId)

  def get(id: String): String = iget(id)(false)

  def get(id: String, fillers: Any*): String = iget(id, fillers: _*)(false)

  def get(showId: Boolean, id: String, fillers: Any*): String = iget(id, fillers: _*)(showId)

  def getWithId(id: String, fillers: Any*): String = iget(id, fillers: _*)(true)

  protected def iget(id: String, fillers: Any*)(showId: Boolean): String = {
    val orig = getResourceBundle getString id
    val origid = if (showId) "[" + id + "] " + orig
    else orig

    val bits = origid.split(PLACE_HOLDER)
    if (bits.length == 1) origid // no holders so no need to split
    else {
      val str = new StringBuilder

      // max up the fillers to match the number of bits
      val gfillers: Seq[Any] = if (fillers.size < bits.size)
        fillers ++ (Array.fill[Any](bits.size - fillers.size)("XXX"))
      else fillers

      // for each of the chunks replace with the filler
      for { pair <- bits zip gfillers }
        str.append(pair._1).append(pair._2)

      str.toString
    }
  }
}

import java.text.{ NumberFormat, DateFormat }

class Localised(locale: Locale) {
  def currency() = NumberFormat.getCurrencyInstance(locale)
  def number() = NumberFormat.getInstance(locale)
  def integer() = NumberFormat.getIntegerInstance(locale)
  def percent() = NumberFormat.getPercentInstance(locale)

  def calender() = Calendar.getInstance(locale)
  def dateFormat(style: Int) = DateFormat.getDateInstance(style, locale)
  def timeFormat(style: Int) = DateFormat.getTimeInstance(style, locale)
  def dateTimeformat(dateStyle: Int, timeStyle: Int) = DateFormat.getDateTimeInstance(dateStyle, timeStyle, locale)
}

trait LocalisedFunctions {
  def localised(implicit locale: () => Locale) = new Localised(locale())
  def $(implicit locale: () => Locale) = localised(locale).currency()
  def %(implicit locale: () => Locale) = localised(locale).percent()

  def localisedTexts(texts: Iterable[(String, Seq[Any])])(implicit resource: Resource) =
    texts.map { p => resource(p._1, p._2) }
  def localisedITexts(texts: Iterable[(String, Seq[Any])])(implicit resource: Resource) =
    texts.map { p => resource(true, p._1, p._2) }

  import java.nio.charset.Charset

  val UTF_8 = Charset.forName("UTF-8")
  val UTF_16 = Charset.forName("UTF-16")
  val UTF_32 = Charset.forName("UTF-32")
  val US_ASCII = Charset.forName("US-ASCII")
  val LATIN = Charset.forName("ISO-8859-1")

  /**
   * A usable default of UTF8 NOT the vm's Charset.defaultCharset based on its locale, use vmDefaultCharset for that
   */
  val defaultCharset = UTF_8

  /**
   * The jvm's default charset, based on system/machine/locale
   */
  val vmDefaultCharset = Charset.defaultCharset()
}

trait LocalisedImplicits {
  implicit def dateToString(date: Date)(implicit locale: () => Locale) =
    localised(locale).dateTimeformat(DateFormat.FULL, DateFormat.FULL).format(date)

  implicit def justClass(clazz: Class[_]): (Class[_], Option[String]) = (clazz, None)
  implicit def toClass(obj: AnyRef): (Class[_], Option[String]) = (obj.getClass(), None)
  implicit def symbolToStr(sym: Symbol) = sym.name
}
