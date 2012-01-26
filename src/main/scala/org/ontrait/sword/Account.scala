package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.oauth.OAuth._
import dispatch.liftjson.Js._
import net.liftweb.json._

object Authentication {
	def apply(username: String, password: String) = new AuthenticationBuilder(username, password)

	private[sword] class AuthenticationBuilder(username: String, password: String) extends ObjectQueryMethod {
		def complete = _ / "account.json" / "authenticate"/ username <<? Map("password" -> password)
	}

	val message = 'message ? str
	val token = 'token ? str
	val userId = 'userId ? int
	val userSignature = 'userSignature ? str  
}

object ApiTokenStatus {
	def apply(apiKey: String) = new ObjectQueryMethod {
		def complete = _ / "account.json" / "apiTokenStatus" <<? Map("api_key" -> apiKey)
	}

	val valid = 'valid ? bool
	val token = 'token ? str
	val expireDate = 'expiresInMillis ? date
	val remainingCalls = 'remainingCalls ? int
	val totalRequests = 'totalRequests ? int
	val resetsInMillis = 'resetsInMillis ? int
}

