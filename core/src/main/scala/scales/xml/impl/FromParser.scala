package scales.xml.impl

/**
 * Indicates if the Scales code is being called from within a parser or not.
 * Only fromParserDefault is available with type NotFromParser from outside of scales.xml
 */ 
sealed trait FromParser

private[xml] case object IsFromParser extends FromParser

private[xml] case object NotFromParser extends FromParser
