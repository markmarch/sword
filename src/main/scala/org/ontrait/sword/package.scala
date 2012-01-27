package org.ontrait

import dispatch.liftjson.Js._

package object sword {
  // date format for wordnik api
  val wordnikDateFormat = "yyyy-MM-DD'T'HH:mm:ss.SSSZZZZ"
  val wdate = datestr(wordnikDateFormat)

  // taken form lift json library
  class ThreadLocal[A](init: => A) extends java.lang.ThreadLocal[A] with (() => A) {
    override def initialValue = init
    def apply = get
  }
}