package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import liftjson.Js._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

class AccountSpec extends Specification {
  import SwordUtils._
  val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))
  val http = new Http
  val props = getProps

  // Authentication test would invlaid the auth token stroed for other test
  // "Authentication" should {
  //   "verify user crendentials" in {
  //     val http = new Http
  //     val token = http(apiClient(Authentication("sword", "erhuaming")) ># (Authentication.token)).head
  //     token.length must be > (0)
  //     props.setProperty("wordnik.auth.token", token)

  //     { 
  //       http(apiClient(Authentication("sword", "wrongpassword")) ># (Authentication.message))
  //     } must throwA[Exception].like {
  //       case e => e.getMessage must contain ("Invalid password")
  //     }

  //     {
  //       http(apiClient(Authentication("swordabc", "wrongpassword")) ># (Authentication.message))
  //     } must throwA[Exception].like {
  //       case e => e.getMessage must contain ("user not found")
  //     }
  //   }
  // }

  "ApiTokenStatus" should {
    "check api token status" in {
      val client = new WordnikClient{
        def apply(block: Request => Request): Request = block(host)
      }
      val valid = http(client(ApiTokenStatus(loadProperty("wordnik.api.key", "")))># (ApiTokenStatus.valid)).headOption
      valid.get must beTrue

      {
        http(client(ApiTokenStatus("somerandomtext")) ># (ErrorResponse.message))
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("unauthorized")
      }
    }
  }

  "User" should {
    "fetch user info" in {
      val username = http(apiClient(User(loadProperty("wordnik.auth.token", ""))) ># ( User.username )).headOption
      username.get must be equalTo ("sword")

      {
        http(apiClient(User("wrongtoken")) ># (ErrorResponse.message)) 
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("User not logged in")
      }
    }
  }

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
      val res = http(apiClient.handle(WordLists(loadProperty("wordnik.auth.token"))))
      res.size aka "nuber of lists" must be > (1)
      res map { WordList.get } forall { _.isRight } must beTrue
      res map { WordList.get } forall { _.right.get.username == "sword"} must beTrue
    }

    "limit fetched user's word lists to be less or equal to 1" in {
      val res = http(apiClient.handle(WordLists(loadProperty("wordnik.auth.token")).limit(1)))
      res.size aka "number of lists" must be > (0)
      res.size aka "number of lists" must be < (2)
    }
  }
}