package progscala2.introscala

object HelloWorld {
  def main(args: Array[String]) = {
    val output = args.map(s => s.toUpperCase).mkString(" ")
    println(output)
    println(HelloWorld.upper("Hello", "This is my", "first scala program"))
  }

  def upper(strings: String*) = {
    strings.map(_.toUpperCase).mkString(", ")
  }
}
