package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.oauth.OAuth._
import dispatch.liftjson.Js._
import net.liftweb.json._

import java.util.Date

object Authenticate extends Extractor[Authenticate] {
  def apply(username: String, password: String) =
    new AuthenticateBuilder(username, password)

  private[sword] class AuthenticateBuilder(username: String, password: String,
    method: String = "GET") extends ObjectQueryMethod {

    def complete = new Function1[Request, Request]{
      def apply(r: Request) = method match {
        case "GET" => r / "account.json" / "authenticate" / username <<?
          Map("password" -> password)
        case _ => r / "account.json" / "authenticate" / username << password
      }
    }

    def POST = new AuthenticateBuilder(username, password, "POST")
  }

  val token = 'token ? str
  val userId = 'userId ? int
  val userSignature = 'userSignature ? str
}

case class Authenticate(token: String, userId: Long, userSignature: String)

object ApiTokenStatus extends Extractor[ApiTokenStatus]{
  def apply(apiKey: String) = new ObjectQueryMethod {
    def complete = _ / "account.json" / "apiTokenStatus" <<?
      Map("api_key" -> apiKey)
  }

  val valid = 'valid ? bool
  val token = 'token ? str
  val expiresInMillis = 'expiresInMillis ? date
  val remainingCalls = 'remainingCalls ? int
  val totalRequests = 'totalRequests ? int
  val resetsInMillis = 'resetsInMillis ? int
}

case class ApiTokenStatus(
  valid: Boolean,
  token: String,
  expiresInMillis: Long,
  remainingCalls: Int,
  totalRequests: Int,
  resetsInMillis: Long
)

object User extends Extractor[User] {
  def apply(authToken: String) = new ObjectQueryMethod {
    def complete = _ / "account.json" / "user" <<?
      Map("auth_token" -> authToken)
  }

  val id = 'id ? int
  val username = 'username ? str
  val status = 'status ? int
  val email = 'email ? str
}

case class User(id: Long, username: String, status: Int, email: String)


