package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import dispatch.liftjson._
import net.liftweb.json._

class WordSpec extends Specification {
  import SwordUtils._

  val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))
  val http = new Http

  "TopExample" should {
    "get the top example for word 'fire'" in {
      val res = http(apiClient.handle(TopExample("fire")))
      Example.get(res) match {
        case Right(example) => example.text must contain ("fire")
        case Left(t) => log(t.getMessage); 1 must be > (1)
      }
    }  
  }

  "Examples" should {
    "list all examples of 'fire'" in {
      val examples = http(apiClient(Examples("fire")) ># (Examples.examples))
      examples.size must be > (0)
      examples.flatMap(Example.text) forall {_.toLowerCase.contains("fire")} must beTrue 

      examples.map(json => Example.get(json).right.get) forall {_.text.toLowerCase.contains("fire")} must beTrue
    }
  }

  "Example" should {
    "extract Example object from json" in {
      val json = parse("""
        {"year":2006,"provider":{"name":"simonschuster","id":722},
        "url":"http://books.simonandschuster.com/9781416540953","word":"fire",
        "text":"Mohamed understood the word fire and said yes, it was fire.",
        "title":"The Blog of War",
        "exampleId":980553966,"rating":758.8301,"documentId":32479185}
        """)
      Example.get(json) match {
        case Right(e) => e.exampleId must be equalTo (980553966L)
        case Left(e) => log(e.getMessage); 1 must be > (1)
      }
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