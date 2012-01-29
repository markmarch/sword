package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.oauth.OAuth._
import dispatch.liftjson.Js._
import net.liftweb.json._

import java.util.Date


  }

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

object User {
  def apply(authToken: String) = new ObjectQueryMethod {
    def complete = _ / "account.json" / "user" <<? Map("auth_token" -> authToken)
  }

  val id = 'id ? int
  val username = 'username ? str
  val status = 'status ? int
  val email = 'email ? str
}

// wordLists
object WordLists {
  def apply(authToken: String) = new WordListsBuilder(Map("auth_token" -> authToken))
  
  private[sword] class WordListsBuilder(params: Map[String, String]) extends ListQueryMethod {
    private def param[T](key: String)(value: T) = new WordListsBuilder(params + (key -> value.toString))
    val skip = param[Int]("skip") _
    val limit = param[Int]("limit") _

    def complete = _ / "account.json" / "wordLists" <<? params
  }  
}

sealed abstract trait Type extends JString
object Public extends JString("PUBLIC") with Type
object Private extends JString("PRIVATE") with Type

object WordList extends Extractor[WordList] {
  val `type` = 'type ? in(Public, Private)
  val userId = 'userId ? int
  val username= 'username ? str
  val permalink = 'permalink ? str
  val numberWordsInList = 'numberWordsInList ? int
  val lastActivityAt = 'lastActivityAt ? wdate
  val updatedAt = 'updatedAt ? wdate
  val createdAt = 'createdAt ? wdate
  val description = 'description ? str
  val name = 'name ? str
  val id = 'id ? int
}

case class WordList(`type`: String, userId: Long, username: String, permalink: String, numberWordsInList: Int, lastActivityAt: Date, updatedAt: Date, createdAt: Date, description: String, name: String, id: Long)


