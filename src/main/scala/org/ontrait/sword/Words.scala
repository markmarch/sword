package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.liftjson._
import dispatch.liftjson.Js._

import net.liftweb.json._

object Search {
  def apply(query: String) = new SearchBuilder(Map()).query(query)

  private[sword] class SearchBuilder(params: Map[String, String]) extends ListQueryMethod {
    private def param[T](key: String)(value: T) = new SearchBuilder(params + (key -> value.toString))

    val query = param[String]("query") _
    val caseSensitive = param[Boolean]("caseSensitive") _
    val includePartOfSpeech = param[String]("includePartOfSpeech") _
    val excludePartOfSpeech = param[String]("excludePartOfSpeech") _
    val minCorpusCount = param[Int]("minCorpusCount") _
    val maxCorpusCount = param[Int]("maxCorpusCount") _
    val minDictionaryCount = param[Int]("minDictionyCount")  _
    val maxDictionaryCount = param[Int]("maxDictionaryCount") _
    val minLength = param[Int]("minLength") _
    val maxLength = param[Int]("maxLength") _
    val skip = param[Int]("skip") _
    val limit = param[Int]("limit") _ 

    def complete = _ / "words.json" / "search" <<? params
  }
}

object SearchResult {
  val count = 'count ? int
  val wordString = 'wordString ? str
}

case class SearchResult(count: Long, wordString: String)

