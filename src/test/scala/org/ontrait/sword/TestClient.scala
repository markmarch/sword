package org.ontrait.sword

import dispatch.Http

trait TestClient {
  import SwordUtils._

  // props
  val props = getProps
  def username = props.getProperty("wordnik.username", "")
  def password = props.getProperty("wordnik.password", "")
  def authToken = props.getProperty("wordnik.auth.token", "")
  def apiKey = props.getProperty("wordnik.api.key", "")

  def setAuthToken(token: String) = props.setProperty("wordnik.auth.token", token)

  val apiClient = ApiClient(apiKey)
  val http = new Http

  def fetch[T](method: Method[T]) = http(apiClient.handle(method))

  def post[T](method: Method[T]) = http(apiClient(method) >|)
}