package org.ontrait.sword

import dispatch._
import dispatch.Request._
import dispatch.liftjson._
import dispatch.liftjson.Js._

import net.liftweb.json._
import java.util.Date

// wordLists
case class WordLists(authToken: String) {
  val contentType = "application/json"

  // get word list by user auth token
  object ByUser {
    def apply() = new ByUserBuilder(Map()).authToken(authToken)

    private[sword] class ByUserBuilder(params: Map[String, String]) extends ListQueryMethod with AuthToken[ByUserBuilder]{
      protected def param[T](key: String)(value: T) = new ByUserBuilder(params + (key -> value.toString))
      val skip = param[Int]("skip") _
      val limit = param[Int]("limit") _

      def complete = _ / "account.json" / "wordLists" <<? params
    }
  }

  object ById {
    def apply(listId: String) = new ObjectQueryMethod {
      def complete = _ / "wordList.json" / listId <<? Map("auth_token" -> authToken)
    }
  }

  object Words {
    def apply(listId: String) = new WordsBuilder(listId, Map()).authToken(authToken)

    private[sword] class WordsBuilder(listId: String, params: Map[String, String]) extends ListQueryMethod with AuthToken[WordsBuilder] with HasOrder[WordsBuilder] {
      protected def param[T](key: String)(value: T) = new WordsBuilder(listId, params + (key -> value.toString))

      val skip = param[Int]("skip") _
      val limit = param[Int]("limit") _

      def complete = _ / "wordList.json" / listId / "words" <<? params
    }
  }

  object Add {
    def apply(listId: String, words: Iterable[String]) = new ObjectQueryMethod {
      def complete = _ / "wordList.json" / listId / "words" << (toJsonStr(words), contentType) <<? Map("auth_token" -> authToken)
    }
  }

  object DeleteWords {
    def apply(listId: String, words: Iterable[String]) = new ObjectQueryMethod {
      def complete = _ / "wordList.json" / listId / "deleteWords" << (toJsonStr(words), contentType) <<? Map("auth_token" -> authToken)
    }
  }

  object Delete {
    def apply(listId: String) = new ObjectQueryMethod {
      def complete = _.DELETE / "wordList.json" / listId <<? Map("auth_token" -> authToken)
    }
  }

  object Update {
    def apply(listId: String, list: String) = new ObjectQueryMethod {
      def complete = new Function1[Request, Request] {
        def apply(r: Request) = {
          r.PUT.copy(body = Some(new RefStringEntity(list, contentType, r.defaultCharset))) / "wordList.json" / listId <<? Map("auth_token" -> authToken)
        }
      }
    }
  }
}

// word list words item
object WordListItem extends Extractor[WordListItem] {
  val userId = 'userId ? int
  val username = 'username ? str
  val numberCommentsOnWord = 'numberCommentsOnWord ? int
  val numberLists = 'numberLists ? int
  val createdAt = 'createdAt ? wdate
  val id = 'id ? int
  val word = 'word ? str
}

case class WordListItem(
  userId: Long,
  username: String,
  numberCommentsOnWord: Int,
  numberLists: Int,
  createdAt: Date,
  id: Long,
  word: String
)

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

case class WordList(
  `type`: String,
  userId: Long,
  username: String,
  permalink: String,
  numberWordsInList: Int,
  lastActivityAt: Date,
  updatedAt: Date,
  createdAt: Date,
  description: String,
  name: String,
  id: Long
) {
  import Serialization._
  implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[WordList])))
  def toJsonStr = write(this)
}
