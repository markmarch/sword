package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.oauth.OAuth._
import dispatch.liftjson.Js._
import net.liftweb.json._

abstract class WordnikClient extends ((Request => Request) => Request) {
  val apiVersion = "v4"
  def hostname = "api.wordnik.com"
  def host: Request = :/ (hostname) / apiVersion
  
  def handle[T](method: Method[T]) = method.defaultHandler(apply(method)) 
}

trait MethodBuilder extends Builder[Request => Request] {
  final def product = setup andThen complete
  def setup = identity[Request] _
  def complete: Request => Request
}

trait Method[T] extends MethodBuilder {
  def defaultHandler: Request => Handler[T]
}

trait WithParams[M] {
  protected def param[T](key: String)(value: T): M
}

trait ObjectQueryMethod extends Method[JValue] {
  def defaultHandler = _ ># identity[JValue]
}

trait ListQueryMethod extends Method[List[JValue]] {
  def defaultHandler = _ ># ary
}

trait Extractor[T] {
  // override default formats to parse wordnik date
  implicit val formats = new DefaultFormats {
    override def dateFormatter = new ThreadLocal(new java.text.SimpleDateFormat(wordnikDateFormat))()
  }

  def get(json: JValue)(implicit manifest: Manifest[T]): Either[Throwable, T] = try {
    Right(json.extract[T])
  } catch {
    case e => Left(e)
  }
}

case class ApiClient(apiKey: String) extends WordnikClient {
  def apply(block: Request => Request): Request = block(host) <<? Map("api_key" -> apiKey)
}

object ErrorResponse {
  val message = 'message ? str
  val errorType = 'type ? str
}