package org.ontrait.sword

import scala.util.logging.ConsoleLogger 
import java.util.Properties
import java.io.{FileInputStream, File}

object SwordUtils extends ConsoleLogger{
	implicit val defaultPropsFile = "default.properties"

	def loadProperty(key: String , defaultValue: String)(implicit propsFile: String): String = {
		println("util:" + new File(propsFile).getAbsolutePath)
		val props = new Properties
		try {
			props.load(new FileInputStream(propsFile))
		} catch {
			case e: Exception => log(e.getMessage)
		}
		props.getProperty(key, defaultValue)
	}	
}