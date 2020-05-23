import scala.io.StdIn
import scala.util.{Failure, Success, Try}

def positive(i: Int): Option[Int] = {
  if (i > 0) Some(i) else None
}

def negative(i: Int): Either[IllegalArgumentException, Int] = {
  if (i<0)
    Right(i)
  else
    Left(new IllegalArgumentException("Positive number"))
}

def divide(): Try[Int] = {
  val dividend = Try(StdIn.readLine("Enter dividend: ").toInt)
  val divisor = Try(StdIn.readLine("Enter divisor: ").toInt)
  dividend.flatMap(x => divisor.map(y => x/y))
}

divide() match {
  case Success(value) => println(s"Result is $value")
  case Failure(exception) => println(s"Error: $exception")
}

val a = positive(5).flatMap(x => positive(10).map(y => x+y))
a match {
  case Some(x) => println(s"sum = $x")
  case None => println("Found negative number")
}

for {
  c <- a
} println(s"c=$c")

for {
  x <- positive(-10)
  y <- positive(12)
} println(s"x+y=${x+y}, $x")

println(s"a = $a")

val t = for {
  x <- negative(-1)
  z <- negative(1)
  y <- negative(-2)
} yield x+y+z

println(t.map(x => positive(-5).map(y => x+y).getOrElse("-")))

t match {
  case Right(value) => println(s"t=$value")
  case Left(error) => println(s"Error: $error")
}

val arr = Array(10,20,30,40,50,60)
for (i <- arr.indices) {
  println(s"($i) -> ${arr(i)}")
}

println("min=" + Math.min(java.lang.Double.MAX_VALUE, 0.00))