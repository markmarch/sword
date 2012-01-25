import org.specs2.mutable._

object HelloWordSpec extends Specification {
  "Hello World" should {
    "say hello to anybody" in {
      HelloWorld.sayHello("World") must beEqualTo("Hello World!")
    }
  }
}
