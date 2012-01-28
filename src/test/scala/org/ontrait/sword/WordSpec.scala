package org.ontrait.sword

import org.specs2.mutable._
import dispatch._
import dispatch.Request._
import dispatch.liftjson.Js._
import dispatch.liftjson._
import net.liftweb.json._

class WordSpec extends Specification with TestClient{
  "TopExample" should {
    "get the top example for word 'fire'" in {
      val res = http(apiClient.handle(TopExample("fire")))
      Example.get(res)  must beRight.like {
         case e: Example => e.text must contain ("fire")
      }
    }  
  }

  "Examples" should {
    "list all examples of 'fire'" in {
      val examples = http(apiClient(Examples("fire")) ># (Examples.examples))
      examples.size must be > (0)
      examples.flatMap(Example.text) forall {_.toLowerCase.contains("fire")} must beTrue 

      examples.map(json => Example.get(json).right.get) forall {_.text.toLowerCase.contains("fire")} must beTrue
    }
  }

  "Example" should {
    "extract Example object from json" in {
      val json = parse("""
        {"year":2006,"provider":{"name":"simonschuster","id":722},
        "url":"http://books.simonandschuster.com/9781416540953","word":"fire",
        "text":"Mohamed understood the word fire and said yes, it was fire.",
        "title":"The Blog of War",
        "exampleId":980553966,"rating":758.8301,"documentId":32479185}
        """)
      Example.get(json) must beRight.like {
        case e: Example => e.exampleId must be equalTo (980553966L)
      }
    }
  }

  "Word" should {
    "extract Word object from json(without suggestions)" in {
      val json = parse("""
        {
          "id": 273356,
          "word": "faces"
        }""")
      Word.get(json) must beRight.like {
        case w: Word => w.word must be equalTo ("faces")
      }
    }

    "extract Word object form json(with suggestions)" in {
      val json = parse("""
        {
          "id": 273356,
          "canonicalForm": "faces",
          "suggestions": [
            "face"
          ],
          "word": "faces"
        }""")
      Word.get(json) must beRight.like {
        case w: Word => 
          w.canonicalForm must beSome.like { 
            case str: String => str must be equalTo "faces"
          }
          w.suggestions must beSome.like { 
            case l: List[String] => l must contain("face")
          }
      }
    }

    "fetch word object" in {
      val res = fetch(Word("face"))
      Word.get(res) should beRight.like {
        case w: Word => w.word must be equalTo ("face")
      }
    }

    "fetch word object use canonicalForm" in {
      val res = fetch(Word("faces").useCanonical(true))
      Word.get(res) should beRight.like {
        case w: Word => w.canonicalForm must beSome
      }
    }
  }

  "Definitions" should {
    "list defintions of word 'contain'" in {
      val definitions = fetch(Definitions("contain"))
      definitions.size must be > (0)
      definitions map { Definition.get } forall { _.isRight } must beTrue
      definitions map { Definition.get } forall {_.right.get.word == "contain"}
    }

    "list at most 4 definitions for word 'contain'" in {
      val definitions = fetch(Definitions("contain").limit(4))
      definitions.size must be > (0)
      definitions.size must be < (5)
      definitions.flatMap(Definition.word) forall { _ == "contain"} must beTrue
    }
  }

  "Pronunciations" should {
    "extract Pronunciation object from json" in {
      val json = parse("""
        {
          "seq": 0,
          "raw": "feɪs",
          "rawType": "IPA"
        }"""
      )
      Pronunciation.get(json) must beRight.like {
        case p: Pronunciation => p must be equalTo (Pronunciation(seq = 0, raw = "feɪs", rawType = "IPA"))
      }
    }  

    "fetch pronunciations for word 'face'" in {
      val res = fetch(Pronunciations("face"))
      res map { Pronunciation.get } forall { _.isRight } must beTrue
      res map { r => Pronunciation.get(r).right.get.rawType } must contain("IPA") 
    }

    "fetch pronunciations for word 'face' from dictionary 'ahd'" in {
      val res = fetch(Pronunciations("face").sourceDictionary("ahd"))
      res must beEmpty
    }

    "fetch pronunciations for word 'face' with limit of 3" in {
      val res = fetch(Pronunciations("face").limit(3))
      res.size must be > (0) and <= (3)
    }
  }

  "Hyphenation" should {
    "extract Hyphenation object from json" in {
      val json = parse("""
        {
          "type": "stress",
          "seq": 0,
          "text": "may"
        }"""
      )
      Hyphenation.get(json) must beRight.like {
        case h: Hyphenation => 
          h must be equalTo (Hyphenation(`type`=Some("stress"), seq=0, text="may"))
      }
    }

    "fetch Hyphenations for word 'maybe'" in {
      val res = fetch(Hyphenation("maybe"))
      res map { Hyphenation.get } forall {_.isRight} must beTrue
      res map { r => Hyphenation.get(r).right.get.text } forall { "maybe".contains(_)} must beTrue
    }
  }

  "Frequency" should {
    "extract Frequency object from json" in {
      val json = parse("""
        {
          "word": "time",
          "totalCount": 1054,
          "frequency": [
            {
              "year": 1900,
              "count": 1054
            }
          ],
          "unknownYearCount": 0
        }"""
      )
      Frequency.get(json) must beRight.like {
        case f: Frequency => 
          f.totalCount must be equalTo (1054)
          f.frequency must contain(YearCount(1900, 1054))
      }
    }  

    "fetch frequency of word 'time'" in {
      val res = fetch(Frequency("time"))
      Frequency.get(res) must beRight.like {
        case f: Frequency => 
          f.totalCount must be > (0) 
          f.frequency.size must be > (0)
      }
    }

    "fetch frequency of word 'time' with start year" in {
      val res = fetch(Frequency("time").startYear(1990))
      Frequency.get(res) must beRight.like {
        case f: Frequency => f.frequency.exists (_.year == 1990) must beTrue
      }
    }

    "fetch frequency of word 'time' with start and end year" in {
      val res = fetch(Frequency("time").startYear(1990).endYear(2000))
      Frequency.get(res) must beRight.like {
        case f: Frequency => f.frequency forall {_.year <= 2000} must beTrue
      }
    }
  }

  "Phrases" should {
    "extract Phrase object from json" in {
      val json = parse("""
        {
          "mi": 13.898796504570253,
          "gram1": "microwave",
          "gram2": "frequencies",
          "wlmi": 13.898796504570253
        }"""
      )
      Phrase.get(json) must beRight.like {
        case p: Phrase => p.gram1 must be equalTo ("microwave")
      }
    }

    "fetch phrases for word 'frequency" in {
      val res = fetch(Phrases("frequency"))
      res map { Phrase.get } forall {_.isRight} must beTrue
    }

    "fetch at most 2 phrases for 'frequency', each with minimal wlmi of 10" in {
      val res = fetch(Phrases("frequency").limit(2).wlmi(10))
      res.size must be > (0) and be <= (2)
      res map { Phrase.get } forall { _.isRight } must beTrue
      res map { Phrase.get} map { _.right.get } forall { _.wlmi >= 10 } 
    }
  }

  "Audios" should {
    "extract Audio object from json(without attributionUrl)" in {
      val json = parse("""
        {
          "commentCount": 0,
          "createdBy": "ahd",
          "createdAt": "2009-03-15T15:32:15.000+0000",
          "id": 21250,
          "word": "face",
          "duration": 0.83,
          "fileUrl": "http://api.wordnik.com/v4/audioFile.mp3/9ef4922bb296141bd4b50b4ce8e7e8857f3974446f5b80787b973f2cba2754d7",
          "audioType": "pronunciation",
          "attributionText": "from The American Heritage® Dictionary of the English Language, 4th Edition"
        }"""
      )
      Audio.get(json) must beRight.like {
        case audio: Audio => audio.createdBy aka "creator of the audio" must be equalTo ("ahd")
      }
    }

    "extract Audio object form json(with attributionUrl)" in {
      val json = parse("""
         {
            "commentCount": 0,
            "description": "see more at http://www.macmillandictionary.com/dictionary/american/face",
            "createdBy": "macmillan",
            "createdAt": "2011-04-10T06:32:38.795+0000",
            "id": 217137,
            "word": "face",
            "duration": 1.04,
            "fileUrl": "http://api.wordnik.com/v4/audioFile.mp3/2802d5867abe18bcfb65886bc46806fce1910bbb49f78dd9dad9a413b6f2afc8",
            "audioType": "pronunciation",
            "attributionText": "definition of face from Macmillan Dictionary -- free online dictionary and thesaurus",
            "attributionUrl": "http://www.macmillandictionary.com/dictionary/american/face"
        }"""
      )
      Audio.get(json) must beRight.like {
        case audio: Audio => 
          audio.attributionUrl.get aka "attributionUrl of the audio" must endWith ("american/face")
      }
    }

    "fetch list of audios for word 'fire'" in {
      val res = fetch(Audios("fire"))
      res map { Audio.get } forall {_.isRight } must beTrue
      res map { r => Audio.get(r).right.get } forall { _.word == "fire" } must beTrue 
    }
  }

  "Realted" should {
    "extract RelatedWords object form json" in {
      val json = parse("""
        {
          "words": [
            "blazing",
            "clean out",
            "smolder",
            "shake",
            "ignition",
            "counterpreparation fire",
            "pop",
            "excite",
            "cookfire",
            "fratricide"
          ],
          "relationshipType": "hyponym"
        }"""
      )  
      RelatedWords.get(json) must beRight.like {
        case r: RelatedWords => 
          r.words.size aka "number of related size" must be > (0)
          r.words must contain ("cookfire")
          r.relationshipType must be equalTo ("hyponym")
      }
    }

    "fetch related words of 'fire'" in {
      val res = fetch(Related("fire"))
      res.size aka "results size" must be > (0)
      res map { RelatedWords.get } forall { _.isRight } must beTrue
    }
  }
}