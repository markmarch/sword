package org.ontrait.sword

import scala.util.logging.ConsoleLogger 
import java.util.Properties
import java.io.{FileInputStream, File}

object SwordUtils extends ConsoleLogger{
  implicit val defaultPropsFile = "default.properties"

  def loadProperty(key: String , defaultValue: String = "")(implicit propsFile: String): String = {
    val props = new Properties
    try {
      props.load(new FileInputStream(propsFile))
    } catch {
      case e: Exception => log(e.getMessage)
    }
    props.getProperty(key, defaultValue)
  } 

  def getProps(implicit propsFile: String): Properties = {
    val props = new Properties
    try {
      props.load(new FileInputStream(propsFile))
    } catch {
      case e: Exception => log(e.getMessage)
    }
    props
  }
}