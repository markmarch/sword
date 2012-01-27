package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import dispatch.liftjson._

class WordSpec extends Specification {
  import SwordUtils._

  val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))
  val http = new Http

  "Examples" should {
    "list all examples of 'fire'" in {
      val examples = http(apiClient(Examples("fire")) ># (Examples.examples))
      examples.size must be > (0)
      examples.flatMap(Example.text) forall {_.toLowerCase.contains("fire")} must beTrue 

      examples.map(json => Example.get(json).right.get) forall {_.text.toLowerCase.contains("fire")} must beTrue
    }
  }

  "Definitions" should {
    "list defintions of word 'contain'" in {
      val definitions = http(apiClient.handle(Definitions("contain")))
      definitions.size must be > (0)
      definitions.map(json => Definition.get(json).right.get) forall {_.word == "contain"} must beTrue
    }

    "list at most 4 definitions for word 'contain'" in {
      val definitions = http(apiClient.handle(Definitions("contain").limit(4)))
      definitions.size must be > (0)
      definitions.size must be < (5)
      definitions.flatMap(Definition.word) forall { _ == "contain"} must beTrue
    }
  }
}