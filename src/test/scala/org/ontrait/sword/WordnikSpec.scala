package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Http
import net.liftweb.json._

class WordnikSpec extends Specification with TestClient{
  import Wordnik._

  val permalink = "freedom--1"
  val list = WordLists(authToken)

  "Wordnik" should {
    "have valid api key" in {
      Wordnik.apiKey.length must be > (0)
    }

    "authenticate user[GET]" in {
      val res = Wordnik.get(Authenticate(username, password))
      Authenticate.get(res) must beRight.like {
        case a: Authenticate => a.token.length must be > (0)
      }
    }

    "authenticate user[POST]" in {
      val res = Wordnik.get(Authenticate(username, password).POST)
      Authenticate.get(res) must beRight.like {
        case a: Authenticate => a.token.length must be > (0)
      }
    }

    "check api token token status" in {
      val res = Wordnik.apiTokenStatus(apiKey)
      ApiTokenStatus.get(res) must beRight.like {
        case s: ApiTokenStatus => s.token must be equalTo(apiKey)
      }
    }

    "get examples for word 'fire' as json" in {
      val res = Wordnik.get(Examples("fire").limit(5))
      val examples = Examples.examples(res)
      examples.size must be > (0) and <= (5)
      examples map { Example.get(_).right.get } forall { _.text.contains("fire") } must beTrue
    }

    "get top example for word 'fire' as json" in {
      val res = Wordnik.get(TopExample("fire"))
      Example.get(res) must beRight.like {
        case e: Example => e.text.toLowerCase must contain("fire")
      }
    }

    "get definitions for word 'fire' as json" in {
      val res = Wordnik.get(Definitions("fire"))
      res.size must be >(0)
      res map { Definition.get(_).right.get } forall { _.word == "fire"} must beTrue
    }

    "get pronunciations for word 'fire' as json" in {
      val res = Wordnik.get(Pronunciations("fire").limit(2))
      res.size must be > (0) and <= (2)
      res map { Pronunciation.get} forall {_.isRight } must beTrue
    }

    "get hyphenations for word 'fire'" in {
      val res = Wordnik.get(Hyphenation("fire"))
      res.size must be > (0)
      res map { Hyphenation.get } forall { _.isRight } must beTrue
    }

    "get frenquency for word 'fire'" in {
      val res = Wordnik.get(Frequency("fire"))
      Frequency.get(res) must beRight.like {
        case f: Frequency => f.word must be equalTo ("fire")
      }
    }

    "get phrases for word 'fire'" in {
      val res = Wordnik.get(Phrases("fire"))
      res.size must be > (0)
      res map { Phrase.get } forall { _.isRight } must beTrue
    }

    "get related words for word 'fire'" in {
      val res = Wordnik.get(Related("fire"))
      res map { RelatedWords.get(_).right.get } forall { _.words.isEmpty == false } must beTrue
    }

    "get audios for word 'fire'" in {
      val res = Wordnik.get(Audios("fire").limit(5))
      res.size must be > (0) and <= (5)
      res map { Audio.get(_).right.get } forall { _.word == "fire"} must beTrue
    }

    "get word object for word 'fire'" in {
      val res = Wordnik.get(Word("fire").useCanonical(true).includeSuggestions(true))
      Word.get(res) must beRight.like {
        case w: Word =>
          w.word must be equalTo ("fire")
          w.suggestions must beSome
      }
    }

    "get word list with permlink of '%s'".format(permalink) in {
      val res = Wordnik.get(WordLists(authToken).ById(permalink))
      WordList.get(res) must beRight.like {
        case l: WordList => l.permalink must be equalTo (permalink)
      }
    }

    "get word lists of user 'sword'" in {
      val res = Wordnik.get(list.ByUser())
      res.size must be > (0)
      res map { WordList.get(_).right.get } forall { _.username == "sword" } must beTrue
    }

    "get words in word list with permalink of '%s'".format(permalink) in {
      val res = Wordnik.get(list.Words(permalink))
      res.size must be > (0)
      res map { WordListItem.get(_).right.get } forall { _.username == "sword" } must beTrue
    }

    "delete words from list with permalink of '%s'".format(permalink) in {
      val res = Wordnik.get(list.DeleteWords(permalink, List("random", "relationship")))
      res must be equalTo (JNothing)
      val updated = Wordnik.get(list.Words(permalink))
      updated map { WordListItem.get(_).right.get.word } must not contain("random", "relationship")
    }

    "add words to word list with permalink of '%s'".format(permalink) in {
      val res = Wordnik.get(list.Add(permalink, List("random", "relationship")))
      res must be equalTo (JNothing)

      // fetch the result and check again
      val updated = Wordnik.get(list.Words(permalink))
      updated map { WordListItem.get(_).right.get.word } must contain("random", "relationship")
    }

    "update the name of list with permalink of '%s'".format(permalink) in {
      val oldList = WordList.get(Wordnik.get(list.ById(permalink))).right.get
      val newName = scala.util.Random.nextInt(10) + "name"
      val updated = oldList.copy(name=newName)

      val res = Wordnik.get(list.Update(permalink, updated.toJsonStr))
      WordList.get(Wordnik.get(list.ById(permalink))) must beRight.like {
        case l: WordList => l.name must be equalTo (newName)
      }
    }

    "search for words with query='fire'" in {
      val res = Wordnik.get(Search("fire").limit(10).maxLength(8))
      res.size must be > (0) and <= (10)
      res map { SearchResult.get(_).right.get} forall { _.wordstring.contains("fire")} must beTrue
    }

    "fetch word of the day" in {
      val res = Wordnik.get(WordOfTheDay())
      WordOfTheDay.get(res) must beRight
    }

    "search(new) for words with query = 'fire'" in {
      val res = Wordnik.get(SearchNew("fire").limit(10).maxLength(8).skip(5))
      val r = SearchResultNew.searchResults(res)
      r.size must be > (0) and <= (10)
      r map { Result.get(_).right.get } forall { _.word.contains("fire")} must beTrue
    }

    "fetch random words" in {
      val res = Wordnik.get(RandomWords().limit(5).minLength(6).maxLength(10))
      res.size must be > (0) and <= (5)
      res map { RandomWord.get(_).right.get.word.length } forall {l => l >=6 && l <= 10}
    }

    "fetch a random word" in {
      val res = Wordnik.get(RandomWord().minLength(10))
      RandomWord.get(res) must beRight.like {
        case w: RandomWord => w.word.length must be >= 10
      }
    }
  }
}