package progscala2.introscala

object OverrideFooHelper {
  implicit class OverrideFoo(s1: Any) {
    def overrideFoo(s: String): Foo = Foo(s"$s1: " + s)
  }
}

object JsonInterpolatorHelper {
  implicit class JsonStringInterpolator(private val sc: StringContext) {
    def json(values: Any*) = {
      val keyRX = """^[\s{,]*(\S+):\s*""".r
      val keys = sc.parts map {
        case keyRX(key) => key
        case str => str
      }

      (keys zip values).toMap
    }
  }
}

case class Foo(s: String)

object Foo {
  implicit def fromString(s: String) = Foo(s)
}

object JSONInterpolatorTest {
  import JsonInterpolatorHelper._
  import OverrideFooHelper._

  val name = "Deep"
  val age = 30
  val book = "Learning"

  def main(strings: Array[String]): Unit = {
    println(json"{name: $name, age: $age, book: $book")

    class test {
      def m(f: Foo) = println(f)
      def m1(s: String) = m(s)
    }

    val t = new test
    t.m1("abc")
    println("abc".overrideFoo("def"))
    println(1.overrideFoo("ghj"))
    println(OverrideFoo("abc").overrideFoo("def"))
  }
}
