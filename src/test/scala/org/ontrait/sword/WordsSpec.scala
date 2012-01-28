package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Request._
import dispatch.liftjson._

import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import java.util.Date

class WordsSpec extends Specification with TestClient {
  "Search" should {
    "fetch search results for word 'parameter'" in {
      val res = fetch(Search("parameter"))
      res map { SearchResult.get } forall { _.isRight } must beTrue
      res map { r => SearchResult.get(r).right.get } forall { _.wordstring.contains("parameter")} must beTrue
    }

    "fetch search results for word 'parameter' with limit of 5" in {
      val res = fetch(Search("parameter").limit(5))
      res.size must be > (0) and <= (5)
      res.map { SearchResult.get } forall { _.isRight } must beTrue
    }

    "fetch search results for word 'parameter' with limit of 5 and skip 2" in {
      def getResult: JValue => SearchResult = SearchResult.get(_).right.get
      val res1 = fetch(Search("parameter").limit(5)) map { getResult }
      val res2 = fetch(Search("parameter").limit(5).skip(2)) map { getResult }

      res1.zip(res2) forall { p => p._1.count >= p._2.count} must beTrue
    }

    "fetch search results for word 'parameter' with max length of 11" in {
      val res = fetch(Search("parameter").maxLength(11))
      res.size must be > (0)
      res map { SearchResult.get } forall { _.isRight } must beTrue
      res map { SearchResult.get(_).right.get } forall { _.wordstring.length <= 11 } must beTrue
    }

    "fetch search results for word 'parameter'(non-noun) with max dictionary sources of 2" in {
      val res = fetch(Search("parameter").excludePartOfSpeech("noun").maxDictionaryCount(2)) 
      res map { SearchResult.get } forall { _.isRight } must beTrue
    }
  }

  "Word Of The Day" should {
    "extract WordOfTheDay object from json" in {
      val json = parse(
        """
        {
          "id": 272321,
          "word": "filibeg",
          "examples": [
            {
              "url": "http://www.gutenberg.org/files/4300/old/ulyss10h.htm",
              "text": "Mr Deasy stared sternly for some moments over the mantelpiece at the shapely bulk of a man in tartan filibegs: Albert Edward, prince of Wales.",
              "title": "Ulysses, by James Joyce",
              "id": 0
            },
            {
              "url": "http://news.google.com/newspapers?id=eP0uAAAAIBAJ&sjid=v9sFAAAAIBAJ&dq=filibeg&pg=5732%2C4621976",
              "text": "Although the kilt is no longer fashionable fighting garb in the British army, it is known that one Scottish unit sailing with the First Division of the Canadian Active Service Force wore the beloved filibeg.",
              "title": "Lone Scottish Unit Wore Kilts Across, Ottawa Citizen, December 19, 1939",
              "id": 0
            },
            {
              "url": "http://query.nytimes.com/mem/archive/pdf?res=F60816FD3F55127A93C4A8178DD85F4D8285F9",
              "text": "The bagpipe, ancient 'woodnote wild' of the Highlands, for centuries as Scottish as the filibeg, haggis, or the sporran, has been forbidden to play at the modern seaside resorts of Largs in Ayrshire.\r",
              "title": "Scotch Resort Bars Bagpipe as Discouraging Patronage, The New York Times, June 16, 1929",
              "id": 0
            }
          ],
          "definitions": [
            {
              "text": "(noun) A plaited petticoat or skirt reaching only to the knees, worn by men in the Highlands of Scotland; a kilt.",
              "partOfSpeech": "noun",
              "source": "century"
            }
          ],
          "contentProvider": {
            "name": "wordnik",
            "id": 711
          },
          "publishDate": "2012-01-27T03:00:00.000+0000",
          "note": "'Filibeg' comes from the Gaelic 'feileadh beag,' with 'feileadh,' meaning 'fold' and 'beag,' meaning 'little.'"
        }
        """
      )

      WordOfTheDay.get(json) must beRight.like {
        case w: WordOfTheDay => w.note must contain ("Gaelic")
      }
    }

    "fetch word of the day of 2010-12-18 from wordnik.com" in {
      val res = fetch(WordOfTheDay().date("2010-12-18"))
      WordOfTheDay.get(res) must beRight
    }

    "fetch word of the day for today" in {
      val res = fetch(WordOfTheDay())
      WordOfTheDay.get(res) must beRight.like {
        case w: WordOfTheDay => 
          w.publishDate.compareTo(new Date()) aka "word of the day must published before now" must be < (0)
      }
    }

    "fetch word of the day with creator named 'sword'" in {
      val res = fetch(WordOfTheDay().creator("sword"))
      WordOfTheDay.get(res) must beLeft.like {
        case e: MappingException => 1 must be equalTo (1)
      }
    }
  }

  "Search New" should {
    "fetch search results for word 'parameter'" in {
      val res = fetch(SearchNew("parameter"))
      SearchResultNew.get(res) must beRight.like {
        case r: SearchResultNew => 
          r.searchResults.size must be > (0)
          r.searchResults forall { _.word.contains("parameter")} must beTrue
      }
    }

    "fetch search results for word 'parameter' with limit of 5" in {
      val res = fetch(SearchNew("parameter").limit(5))
      SearchResultNew.get(res) must beRight.like {
        case r: SearchResultNew => r.searchResults.size must be > (0) and <= (5)
      } 
    }

    "fetch search results for word 'parameter' with limit of 5 and skip 2" in {
      def getResult: JValue => List[Result] = SearchResultNew.get(_).right.get.searchResults
      val res1 = fetch(SearchNew("parameter").limit(5)) 
      val res2 = fetch(SearchNew("parameter").limit(5).skip(2))

      val (l1, l2) = {
        val l = List(res1, res2) map getResult
        (l(0), l(1))
      }
      l1.zip(l2) forall { p => p._1.count >= p._2.count} must beTrue
    }

    "fetch search results for word 'parameter' with max length of 11" in {
      val res = fetch(SearchNew("parameter").maxLength(11))
      SearchResultNew.get(res) must beRight.like {
        case r: SearchResultNew => r.searchResults forall { _.word.length <= 11 } must beTrue
      }
    }

    "fetch search results for word 'parameter'(non-noun) with max dictionary sources of 2" in {
      val res = fetch(SearchNew("parameter").excludePartOfSpeech("noun").maxDictionaryCount(2)) 
      SearchResultNew.get(res) must beRight
    }
  }

  "Random Word" should {
    "extract RandomWord object from json" in {
      val json = parse("""{ "id": 1002, "word": "fire"}""")
      RandomWord.get(json) must beRight.like {
        case w: RandomWord => w must be equalTo RandomWord(1002, "fire")
      }
    }

    "fetch a random word" in {
      val res = fetch(RandomWord())
      RandomWord.get(res) must beRight
    }

    "fetch a random word with min length of 10 and max length of 15" in {
      val res = fetch(RandomWord().minLength(10).maxLength(15))
      RandomWord.get(res) must beRight.like {
        case RandomWord(_, word) => word.length must be >= (10) and <= (15)
      }
    }
  }

  "Random Words" should {
    "extract a list of random words form json" in {
      val json = parse("""[{ "id": 1002, "word": "fire"}, { "id": 1000, "word": "wood"}]""")
      json match {
        case JArray(arr) => arr map { RandomWord.get } forall { _.isRight } must beTrue
        case _ => 1 aka "got a wrong type" must be > (1)
      } 
    }

    "fetch a list of random words" in {
      val res = fetch(RandomWords())
      res.size must be > (0)
      res map { RandomWord.get } forall { _.isRight } must beTrue
    }

    "fetch a list of at most 5 random words with min length of 10 and max length of 15" in {
      val res = fetch(RandomWords().limit(5).minLength(10).maxLength(15))
      res.size must be > (0) and <= (5)
      res map { RandomWord.get } forall { _.isRight } must beTrue
      res map { RandomWord.get(_).right.get } forall { w => w.word.length >= 10 && w.word.length <= 15} must beTrue
    }
  }
}