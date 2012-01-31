package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import net.liftweb.json._

import java.util.Date

// examples
object Examples {
  def apply(word: String) = new ExamplesBuilder(word, Map())

  private[sword] class ExamplesBuilder(word: String, params: Map[String, String])
    extends ObjectQueryMethod {

    private def param[T](key: String)(value: T) =
      new ExamplesBuilder(word, params + (key -> value.toString))

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

  private[sword] class TopExampleBuilder(word: String, params: Map[String, String])
    extends ObjectQueryMethod {

    private def param[T](key: String)(value: T) =
      new TopExampleBuilder(word, params + (key -> value.toString))

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

case class Example(
  provider: Provider,
  year: Int,
  url: String,
  word: String,
  text: String,
  title: String,
  exampleId: Long,
  rating: Double,
  documentId: Long
)

// word
object Word extends Extractor[Word] {
  def apply(word: String) = new WordBuilder(word, Map())

  private[sword] class WordBuilder(word: String, params: Map[String, String])
    extends ObjectQueryMethod {

    def param[T](key: String)(value: T) =
    new WordBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val includeSuggestions = param[Boolean]("includeSuggestions") _

    def complete = _ / "word.json" / word <<? params
  }
  val word = 'word ? str
  val suggestions = 'suggestions ? ary
  val canonicalForm = 'canonicalForm ? str
  val id = 'id ? int
}

case class Word(
  id: Long,
  word: String,
  suggestions: Option[List[String]],
  canonicalForm: Option[String],
  originalWord: Option[String]
)

// definitions
object Definitions {
  def apply(word: String) = new DefinitionsBuilder(word, Map())

  private[sword] class DefinitionsBuilder(word:String, params: Map[String, String])
    extends ListQueryMethod {

    private def param[T](key: String)(value: T) =
      new DefinitionsBuilder(word, params + (key -> value.toString))

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

case class Definition(
  word: String,
  text: String,
  score: Double,
  partOfSpeech: String,
  attributionText: String,
  sourceDictionary: String,
  sequence: String
)

// related
object Related {
  def apply(word: String) = new RelatedBuilder(word, Map())

  private[sword] class RelatedBuilder(word: String, params: Map[String, String])
    extends ListQueryMethod {

    def param[T](key: String)(value: T) =
      new RelatedBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val partOfSpeech = param[String]("partOfSpeech") _
    val sourceDictionary = param[String]("sourceDictionary") _
    val limit = param[Int]("limit") _
    val `type` = param[String]("type") _

    def complete = _ / "word.json" / word / "related" <<? params
  }

}

object RelatedWords extends Extractor[RelatedWords]{
  val words = 'words ? ary
  val relationshipType = 'relationshipType ? str
}

case class RelatedWords(
  words: List[String],
  relationshipType: String
)

// pronunciations
object Pronunciations {
  def apply(word: String) = new PronunciationsBuilder(word, Map())

  private[sword] class PronunciationsBuilder(word: String, params: Map[String, String])
    extends ListQueryMethod {

    private def param[T](key: String)(value: T) =
      new PronunciationsBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val sourceDictionary = param[String]("sourceDictionary") _
    val typeFormat = param[String]("typeFormat") _
    val limit = param[Int]("limit") _

    def complete = _ / "word.json" / word / "pronunciations" <<? params
  }

}

object Pronunciation extends Extractor[Pronunciation] {
  val seq = 'seq ? int
  val raw = 'raw ? str
  val rawType = 'rawType ? str
}

case class Pronunciation(
  seq: Int,
  raw: String,
  rawType: String
)

// hyphenation

object Hyphenation extends Extractor[Hyphenation] {
  def apply(word: String) = new HyphenationBuilder(word, Map())


  private[sword] class HyphenationBuilder(word: String, params: Map[String, String])
    extends ListQueryMethod {

    private def param[T](key: String)(value: T) =
      new HyphenationBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val sourceDictionary = param[String]("sourceDictionary") _
    val limit = param[Int]("limit")_

    def complete = _ / "word.json" / word / "hyphenation" <<? params
  }

  val `type` = 'type ? str
  val seq = 'seq ? int
  val text= 'text ? str
}

case class Hyphenation(
  `type`: Option[String],
  seq: Int,
  text: String
)

// frequency
object Frequency extends Extractor[Frequency] {
  def apply(word: String) = new FrequencyBuilder(word, Map())

  private[sword] class FrequencyBuilder(word: String, params: Map[String, String])
    extends ObjectQueryMethod {

    private def param[T](key: String)(value: T) =
      new FrequencyBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val startYear = param[Int]("startYear") _
    val endYear = param[Int]("endYear") _

    def complete = _ / "word.json" / word / "frequency" <<? params
  }

  val word = 'word ? str
  val totalCount = 'totalCount ? int
  val frequency = 'frequency ? ary
  val unknownYearCount = 'unknownYearCount ? int
}

case class YearCount(year: Int, count: Int)
case class Frequency(
  word: String,
  totalCount: Int,
  frequency: List[YearCount],
  unknownYearCount: Int
)

// phrases

object Phrases {
  def apply(word: String) = new PhrasesBuilder(word, Map())

  private[sword] class PhrasesBuilder(word: String, params: Map[String, String])
    extends ListQueryMethod {

    private def param[T](key: String)(value: T) =
      new PhrasesBuilder(word, params + (key -> value.toString))

    val limit = param[Int]("limit") _
    val wlmi = param[Int]("wlmi") _
    val useCanonical = param[Boolean]("useCanonical") _

    def complete = _ / "word.json" / word/ "phrases" <<? params
  }

}

object Phrase extends Extractor[Phrase] {
  val mi = 'mi ? double
  val gram1 = 'gram1 ? str
  val gram2 = 'gram2 ? str
  val wlmi = 'wlmi ? double
}

case class Phrase(
  mi: Double,
  gram1: String,
  gram2: String,
  wlmi: Double
)

// audios
object Audios {
  def apply(word: String) = new AudiosBuilder(word, Map())

  private[sword] class AudiosBuilder(word: String, params: Map[String, String])
    extends ListQueryMethod {

    private def param[T](key: String)(value: T) =
      new AudiosBuilder(word, params + (key -> value.toString))

    val useCanonical = param[Boolean]("useCanonical") _
    val limit = param[Int]("limit") _

    def complete = _ / "word.json" / word / "audio"
  }
}

object Audio extends Extractor[Audio] {
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

case class Audio(
  id: Long,
  word: String,
  commentCount: Int,
  createdBy: String,
  createdAt: Date,
  duration: Double,
  fileUrl: String,
  audioType: String,
  attributionText: String,
  attributionUrl: Option[String]
)
