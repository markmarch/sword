package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import net.liftweb.json._

object Examples {
  def apply(word: String) = new ExamplesBuilder(word, Map())

  private[sword] class ExamplesBuilder(word: String, params: Map[String, String]) 
    extends ObjectQueryMethod {

    private def param[T](key: String)(value: T) = new ExamplesBuilder(word, params + (key -> value.toString))

    val includeDuplicate = param[Boolean]("includeDuplicate") _
    val contentProvider = param[String]("contentProvider") _
    val useCanonical = param[Boolean]("useCanonical") _
    val skip = param[Int]("skip") _
    val limit = param[Int]("limit") _

    def complete = _ / "word.json" / word / "examples" <<? params
  }

  val examples = 'examples ? ary
}

object TopExample {
  def apply(word: String) = new TopExampleBuilder(word, Map())

  private[sword] class TopExampleBuilder(word: String, params: Map[String, String]) extends ObjectQueryMethod {
    private def param[T](key: String)(value: T) = new TopExampleBuilder(word, params + (key -> value.toString))

    val contentProvider = param[String]("contentProvider") _
    val useCanonical = param[Boolean]("useCanonical") _

    def complete = _ / "word.json" / word / "topExample" <<? params
  }
}

object Example extends Extractor[Example] {
  object Provider extends Obj('provider) {
    val name = this >>~> 'name ? str
    val id = this >>~> 'id ? int 
  }

  val year = 'year ? int
  val url = 'url ? str
  val word = 'word ? str
  val text = 'text ? str
  val title = 'title ? str
  val exampleId = 'exampleId ? int
  val rating = 'rating ? double
  val documentId = 'documentId ? int
}

case class Provider(name: String, id: Int)

case class Example(provider: Provider, year: Int, url: String, word: String, text: String, 
  title: String, exampleId: Long, rating: Double, documentId: Long)

object Definitions {
  def apply(word: String) = new DefinitionsBuilder(word, Map())

  private[sword] class DefinitionsBuilder(word:String, params: Map[String, String]) extends ListQueryMethod {
    private def param[T](key: String)(value: T) = new DefinitionsBuilder(word, params + (key -> value.toString))

    val limit = param[Int]("limit") _
    val partOfSpeech = param[String]("partOfSpeech") _
    val includeRelated = param[Boolean]("includeRelated") _
    val sourceDictionaries = param[String]("sourceDictionaries") _
    val useCanonical = param[Boolean]("useCanonical") _
    val includeTag = param[String]("includeTag") _

    def complete = _ / "word.json" / word / "definitions" <<? params
  }
}

object Definition extends Extractor[Definition]{
  val word = 'word ? str
  val text = 'text ? str
  val score = 'score ? double
  val partOfSpeech = 'partOfSpeech ? str
  val attributionText = 'attributionText ? str
  val sourceDictionary = 'sourceDictionary ? str
  val sequence = 'sequence ? int
}

case class Definition(word: String, text: String, score: Double, partOfSpeech: String, 
  attributionText: String, sourceDictionary: String, sequence: String)

object Related extends Extractor[RelatedWords] {
  def apply(word: String) = new RelatedBuilder(word, Map())

  private[sword] class RelatedBuilder(word: String, params: Map[String, String]) extends ListQueryMethod {
    def param[T](key: String)(value: T) = new RelatedBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val partOfSpeech = param[String]("partOfSpeech") _
    val sourceDictionary = param[String]("sourceDictionary") _
    val limit = param[Int]("limit") _
    val `type` = param[String]("type") _

    def complete = _ / "word.json" / word / "related" <<? params
  }
}

object RelatedWords {
  val words = 'words ? ary
  val relationshipType = 'relationshipType ? str
}

case class RelatedWords(words: List[String], relationshipType: String)

object Audios extends Extractor[Audio]{
  def apply(word: String) = new AudiosBuilder(word, Map())

  private[sword] class AudiosBuilder(word: String, params: Map[String, String]) extends ListQueryMethod {
    private def param[T](key: String)(value: T) = new AudiosBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val limit = param[Int]("limit") _

    def complete = _ / "word.json" / word / "audio"
  }
}

object Audio {
  val commentCount = 'commentCount ? int
  val createdBy = 'createdBy ? str
  val createdAt = 'createdAt ? wdate
  val id = 'id ? int
  val word = 'word ? str
  val duration = 'duration ? double
  val fileUrl = 'fileUrl ? str
  val audioType = 'audioType ? str
  val attributionText = 'attributionText ? str
  val attributionUrl = 'attributionUrl ? str
}

case class Audio(id: Long, commentCount: Int, createdBy: String, createdAt: String,
  duration: Double, fileUrl: String, audioType: String, attributionText: String, 
  attributionUrl: Option[String])

