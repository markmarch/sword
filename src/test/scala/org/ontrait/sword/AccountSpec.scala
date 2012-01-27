package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import liftjson.Js._

class AccountSpec extends Specification {
  import SwordUtils._
  val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))
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
      val http = new Http
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
      val http = new Http
      val username = http(apiClient(User(loadProperty("wordnik.auth.token", ""))) ># ( User.username )).headOption
      username.get must be equalTo ("sword")

      {
        http(apiClient(User("wrongtoken")) ># (ErrorResponse.message)) 
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("User not logged in")
      }
    }
  }
}