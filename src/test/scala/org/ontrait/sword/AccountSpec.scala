package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import liftjson.Js._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

class AccountSpec extends Specification  with TestClient {
  import SwordUtils._

  // Authentication test would invlaid the auth token stroed for other test
  "Authenticate" should {
    "verify user crendentials(GET)" in {
      val res = fetch(Authenticate(username,  password))
      Authenticate.get(res) must beRight.like {
        case auth: Authenticate => 
          setAuthToken(auth.token)
          storeProps(props)
          auth.token.length must be > (0)
      }

      { 
        fetch(Authenticate("sword", "wrongpassword"))
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("Invalid password")
      }

      {
        fetch(Authenticate("_random_no_&abc", "admin"))
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("user not found")
      }
    }

    "verify user crendentials(POST)" in {
      val res = fetch(Authenticate(username, password).POST)
      Authenticate.get(res) must beRight.like {
        case auth: Authenticate => 
          setAuthToken(auth.token)
          storeProps(props)
          auth.token.length must be > (0)
      }
    }
  }

  "ApiTokenStatus" should {
    "check api token status" in {
      val client = new WordnikClient{
        def apply(block: Request => Request): Request = block(host)
      }
      val res = http(client.handle(ApiTokenStatus(apiKey)))
      ApiTokenStatus.get(res) must beRight.like {
        case s: ApiTokenStatus => s.valid must beTrue
      }

      {
        http(client.handle(ApiTokenStatus("somerandomtext")))
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("unauthorized")
      }
    }
  }

  "User" should {
    "fetch user info" in {
      val res = fetch(User(authToken))
      User.get(res) must beRight.like {
        case u: User => u.username must be equalTo ("sword")
      }

      {
        fetch(User("wrong_token"))
      } must throwA[Exception].like {
        case e => e.getMessage must contain ("User not logged in")
      }
    }
  }
}