package org.ontrait.sword

import dispatch._
import json._
import JsHttp._
import oauth._
import oauth.OAuth._
import dispatch.Request._

object Wordnik {
	private[sword] var wordnikApiKey: String = _

	val host = :/("api.wordnik.com")
	val apiV4 = host / "v4"

	// init Wordnik with an API key
	def init(apiKey: String) {
		wordnikApiKey = apiKey
	}

	def init() {
		// try to init by loading api from default properties
		import SwordUtils._
		wordnikApiKey = loadProperty("wordnik.api.key", "")
	}
}

object Word extends Request(Wordnik.apiV4 / "word.json") {
	def apply(word: String) = this / "word.json" / word <<? Map("api_key" -> Wordnik.wordnikApiKey)	
}

object Definitions extends Js {
	def apply(word: String, params: (String, String)*) = new DefinitionsBuilder(word, Map(params: _*), Map.empty[String, String])

	class DefinitionsBuilder(word: String, params: Map[String, String], headers: Map[String, String]) extends Builder[Handler[List[JsObject]]] {
		private def param(key: String)(value: Any) = new DefinitionsBuilder(word, params + (key -> value.toString), headers)
		private def header(key: String)(value: Any) = new DefinitionsBuilder(word, params, headers + (key -> value.toString))

		val limit = param("limit")_
		val partOfSpeech = param("partOfSpeech")_
		val includeRelated = param("includeRelated")_
		val sourceDictionaries = param("sourceDictionaries")_
		val useCanonical = param("useCanonical")_
		val includeTags = param("includeTags")_

		def product = Word(word) / "definitions" <<? params <:< headers ># ('results ! (list ! obj))
	}

	val word = 'word ? str
	val text = 'text ? str
	val score = 'score ? num
	val partOfSpeech = 'partOfSpeech ? str
	val attributionText = 'attributionText ? str
	val sourceDictionary = 'sourceDictionary ? str
	val sequence = 'sequence ? num
}

