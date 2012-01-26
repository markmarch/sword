package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import liftjson.Js._

class AccountSpec extends Specification {
	import SwordUtils._
	val apiClient = ApiClient(loadProperty("wordnik.api.key", ""))

	"Authentication" should {
		"verify user crendentials" in {
			val http = new Http
			val token = http(apiClient(Authentication("sword", "erhuaming")) ># (Authentication.token)).headOption
			token.get.length must be > (0)

			{ 
				http(apiClient(Authentication("sword", "wrongpassword")) ># (Authentication.message))
			} must throwA[Exception].like {
				case e => e.getMessage must contain ("Invalid password")
			}

			{
				http(apiClient(Authentication("swordabc", "wrongpassword")) ># (Authentication.message))
			} must throwA[Exception].like {
				case e => e.getMessage must contain ("user not found")
			}
		}
	}

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
}