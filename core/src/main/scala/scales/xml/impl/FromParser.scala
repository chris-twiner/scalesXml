package scales.xml.impl

/**
 * Indicates if the Scales code is being called from within a parser or not.
 * Only fromParserDefault is available with type NotFromParser and the NotFromParser object itself from outside of scales.xml
 */ 
sealed trait FromParser

private[xml] case object IsFromParser extends FromParser

/**
 * NotFromParser may be used explicitly to provide a FromParser instance in user code.  Alternatively use the implciit ScalesXml.fromParserDefault.
 */
case object NotFromParser extends FromParser
