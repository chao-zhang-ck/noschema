package org.datacrafts.logging

/**
  * Slf4j is compatible with several widely used jvm logging solutions
  * It can automatically picks up configurations, without requiring configuration in code
  */
trait Slf4jLogging extends Logging {

  // Method to get the logger name for this object
  protected def dataCraftsLogName = {
    // Ignore trailing $'s in the class names for Scala objects
    this.getClass.getName.stripSuffix("$")
  }

  // the compatible logger implementations are injected without parameter
  @transient override lazy val dataCraftsLogger: org.slf4j.Logger =
    org.slf4j.LoggerFactory.getLogger(dataCraftsLogName)

}
