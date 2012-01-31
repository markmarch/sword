package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Request._
import dispatch.liftjson._
import dispatch.liftjson.Js._
import net.liftweb.json._
import java.util.Date

class WordListSpec extends Specification with TestClient {
  val permalink = "freedom--1"

  "WordLists" should {
    "extract WordList object from json" in {
      val json = parse("""
        {
          "type":"PRIVATE",
          "userId":1082314,
          "username":"sword",
          "permalink":"test--44",
          "numberWordsInList":1,
          "lastActivityAt":"2012-01-27T20:21:20.138+0000",
          "updatedAt":"2012-01-27T20:21:20.166+0000",
          "createdAt":"2012-01-27T20:21:20.138+0000",
          "description":"testing",
          "name":"test",
          "id":71020
        }""")
      WordList.get(json) match {
        case Right(wordList) => wordList.username aka "username of the wordlist" must be equalTo "sword"
        case Left(e) => 1 aka e.getMessage must be > (1)
      }
    }

    "fetch user's word lists" in {
      val res = fetch(WordLists(authToken).ByUser())
      res.size aka "nuber of lists" must be > (0)
      res map { WordList.get } forall { _.isRight } must beTrue
      res map { WordList.get(_).right.get } forall { _.username == "sword"} must beTrue
    }

    "limit fetched user's word lists to be less or equal to 1" in {
      val res = fetch(WordLists(authToken).ByUser().limit(1))
      res.size aka "number of lists" must be > (0)
      res.size aka "number of lists" must be < (2)
    }

    "fetches words in a WordList and sort by alpha" in {
      val res = fetch(WordLists(authToken).Words(permalink).sortBy(Alpha))
      res.size must be > (0)
      res map { WordListItem.get(_).right.get } forall {_.username == "sword"}
    }

    "add words to a word list" in {
      fetch(WordLists(authToken).Add(permalink, List("random")))
      success
    }

    "fetchs a word list by id" in {
      val res = fetch(WordLists(authToken).ById(permalink))
      WordList.get(res) must beRight.like {
        case l: WordList => l.permalink must be equalTo (permalink)
      }
    }

    "delete words from a word list" in {
      fetch(WordLists(authToken).DeleteWords(permalink, List("random")))
      success
    }

    "update a word list" in {
      val name = util.Random.nextInt(10) + "name"
      val old = fetch(WordLists(authToken).ById(permalink))
      val list = WordList.get(old).right.get
      fetch(WordLists(authToken).Update(permalink, list.copy(name=name).toJsonStr))

      val res = fetch(WordLists(authToken).ById(permalink))
      WordList.get(res) must beRight.like {
        case wl: WordList => wl.name must be equalTo (name)
      }
    }
  }

  "WordListItem" should {
    "extract WordListItem object from json" in {
      val json = parse("""{"userId":1082314,"username":"sword","numberCommentsOnWord":2,"numberLists":32,"createdAt":"2012-01-28T23:24:38.782+0000","id":1120557,"word":"sword"}""")
      WordListItem.get(json) must beRight.like {
        case i: WordListItem => i.id must be_== (1120557L)
      }
    }
  }
}