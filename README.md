###sword
Scala library for wordnik REST API.

[Wordnik Developer](http://developer.wordnik.com)

### Project setup

Currently I haven't publish any jar files yet(I plan to do so soon). If you use sbt as your build tool, you can add source dependency as:

```scala
object MyApp extends Build {
  lazy val root = Project("root", file(".")) dependsOn(sword)

  // wordnik scala library sword
  lazy val sword = uri("git://github.com/markmarch/sword#0.1")
}
```

### Usage
All version 4 of Wordnik api is supported. See [Wordnik API doc](http://developer.wordnik.com/docs) for all avaiable APIs.

You need to get your api key at [Wordnik Developer](http://developer.wordnik.com). And put the api in a `.properties` file at the root of the project(the same foler as `src`), for examples:

```properties
wordnik.api.key=XXXXXX
wordnik.username=XXXXX
wordnik.password=XXXX
wordnik.auth.token=XXXXX
```

You need to set up `wordnik.username`, `wordnik.password` and `wordnik.auth.token` to run the tests.

`sword` uses [dispatch](http://dispatch.databinder.net/Dispatch.html).

A sample app:

```scala
import org.ontrait.sword._
import dispatch._

object HelloWordnik extends App {
  
  // get the definitions for a word
  val res = Wordnik.get(Definitions("fire").limit(5)) // will fetch get json response
  val definitions = res flatMap { Definition.text } // extract definitions 
  definitions foreach println
  
  // to use different Executor(see http://dispatch.databinder.net/Dispatch.html to learn more about Executor)
  val apiKey = "put you api key here"
  val api = new WordnikAPI(apiKey, new Http)
  
  // get the top example for a word
  val example = Example.get(api.get(TopExample("fire")) match {
    case Right(e) => println(e.text)
    case Left(e) => print(e.getMessage)
  }
  
}
```

For more example usage, see [test code](https://github.com/markmarch/sword/tree/master/src/test/scala/org/ontrait/sword)
  
This library is largely based on [n8han](https://github.com/n8han)'s [dispatch-meetup](https://github.com/n8han/dispatch-meetup).
