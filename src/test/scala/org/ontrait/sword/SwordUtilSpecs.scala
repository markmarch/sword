package org.ontrait.sword

import scala.util.logging.ConsoleLogger
import org.specs2.mutable._
import java.io.File

class SwordUtilsSpecs extends Specification{
  "Sword Utils" should {
    import SwordUtils._

    "load property from property file" in {
      val nonExistsFile = "nonExistsFile.properties"
      val defaultFile = "default.properties"

      loadProperty("wordnik.api.key", "") must not be equalTo("")

      loadProperty("wordnik.api.key", "")(defaultFile) must not be equalTo("")

      loadProperty("wordnik.api.key", "")(nonExistsFile) must be equalTo("")

      loadProperty("strange.key", "") must be equalTo("")
    }
  }
}