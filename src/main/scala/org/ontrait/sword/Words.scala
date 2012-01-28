package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.liftjson._
import dispatch.liftjson.Js._

import net.liftweb.json._
import java.util.Date

trait SearchCriteria[T] extends WithParams[T] {
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
}

// search
object Search {
  def apply(query: String) = new SearchBuilder(Map()).query(query)

  private[sword] class SearchBuilder(params: Map[String, String]) extends ListQueryMethod with SearchCriteria[SearchBuilder] {
    protected def param[T](key: String)(value: T) = new SearchBuilder(params + (key -> value.toString))

    val query = param[String]("query") _
    
    def complete = _ / "words.json" / "search" <<? params
  }
}

object SearchResult extends Extractor[SearchResult]{
  val count = 'count ? int
  val wordString = 'wordString ? str
}

case class SearchResult(count: Long, wordstring: String)

// search new
object SearchNew {
  def apply(word: String) = new SearchBuilder(word, Map())

  private[sword] class SearchBuilder(word: String, params: Map[String, String]) extends ObjectQueryMethod with SearchCriteria[SearchBuilder]{
    protected def param[T](key: String)(value: T) = new SearchBuilder(word, params + (key -> value.toString))

    def complete = _ / "words.json" / "search" / word <<? params
  }
}

object SearchResultNew extends Extractor[SearchResultNew] {
  val totalResults = 'totalResults ? int
  val searchResults = 'searchResults ? ary

  val lexicality = 'lexicality ? double
  val count = 'count ? int
  val word = 'word ? str
}

case class Result(lexicality: Double, count: Int, word: String)
case class SearchResultNew(totalResults: Int, searchResults: List[Result])

// word of the day
object WordOfTheDay extends Extractor[WordOfTheDay] {
  def apply() = new WordOfTheDayBuilder(Map())

  private[sword] class WordOfTheDayBuilder(params: Map[String, String]) extends ObjectQueryMethod {
    def param[T](key: String)(value: T) = new WordOfTheDayBuilder(params + (key -> value.toString))

    val date = param[String]("date") _
    val category = param[String]("category") _
    val creator = param[String]("creator") _

    def complete = _ / "words.json" / "wordOfTheDay" <<? params
  }

  val id = 'id ? int
  val word = 'word ? str
  val examples = 'examples ? ary
  val definitions = 'definitions ? ary
  val contentProvider = 'contentProvider ? obj
  val publishDate = 'publishDate ? wdate
  val note = 'note ? str
}

case class WOTAExample(url: String, text: String, title: String, id: Int)
case class WOTADefinition(text: String, partOfSpeech: String, source: String)
case class ContentProvider(name: String, id: Long)

case class WordOfTheDay(id: Long, word: String, examples: List[WOTAExample], definitions: List[WOTADefinition], contentProvider: ContentProvider, publishDate: Date, note: String)

// random words
trait RandomCriteria[T] extends WithParams[T] {
  val hasDictionaryDef = param[Boolean]("hasDictionaryDef") _
  val includePartOfSpeech = param[Boolean]("includePartOfSpeech") _
  val excludePartOfSpeech = param[Boolean]("excludePartOfSpeech") _
  val minCorpusCount = param[Int]("minCorpusCount") _
  val maxCorpusCount = param[Int]("maxCorpusCount") _
  val minDictionyCount = param[Int]("minDictionyCount") _
  val maxDictionaryCount = param[Int]("maxDictionaryCount") _
  val minLength = param[Int]("minLength") _
  val maxLength = param[Int]("maxLength") _
}

object RandomWord extends Extractor[RandomWord] {
  def apply() = new RandomWordBuilder(Map())

  private[sword] class RandomWordBuilder(params: Map[String, String]) extends ObjectQueryMethod with RandomCriteria[RandomWordBuilder] {
    protected def param[T](key: String)(value: T) = new RandomWordBuilder(params + (key -> value.toString))

    def complete = _ / "words.json" / "randomWord" <<? params
  }

  val id = 'id ? int
  val word = 'word ? str
}

case class RandomWord(id: Long, word: String)

object RandomWords {
  def apply() = new RandomWordsBuilder(Map())

  private[sword] class RandomWordsBuilder(params: Map[String, String]) extends ListQueryMethod with RandomCriteria[RandomWordsBuilder] {
    protected def param[T](key: String)(value: T) = new RandomWordsBuilder(params + (key -> value.toString))

    val sortBy = param[String]("sortBy") _
    val sortOrder = param[String]("sortOrder") _
    val limit = param[Int]("limit") _

    def complete = _ / "words.json" / "randomWords" <<? params
  }
}