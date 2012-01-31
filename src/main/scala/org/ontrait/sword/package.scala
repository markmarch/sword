package org.ontrait

import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

package object sword {
  // date format for wordnik api
  val wordnikDateFormat = "yyyy-MM-DD'T'HH:mm:ss.SSSZZZZ"
  val wdate = datestr(wordnikDateFormat)

  // taken form lift json library
  class ThreadLocal[A](init: => A) extends java.lang.ThreadLocal[A] with (() => A) {
    override def initialValue = init
    def apply = get
  }

  def toJsonStr(words: Iterable[String]): String = words.map("""{"word": "%s"}""".format(_)).mkString("[", ",", "]")
}