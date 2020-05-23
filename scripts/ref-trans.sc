def addInts(a: String, b: String): Either[NumberFormatException, Int] = {
  try {
    Right(a.toInt + b.toInt)
  } catch {
    case nfe: NumberFormatException => Left(nfe)
  }
}

val a = for {
  x <- addInts("1", "2")
  y <- addInts("2", "3")
  z <- addInts("3", "4")
} yield x+y+z

val c:Int = a.getOrElse(-1)
println(c)
println(addInts("1", "2").getOrElse("-"))
println(addInts("a", "1").getOrElse("-"))