package org.ontrait.sword

import dispatch.Http

trait TestClient {
  import SwordUtils._

  val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))
  val http = new Http

  def fetch[T](method: Method[T]) = http(apiClient.handle(method))
}