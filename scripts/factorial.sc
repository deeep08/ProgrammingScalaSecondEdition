import scala.annotation.tailrec

def factorial(n: Int): Long = {
  @tailrec
  def fact(n: Int, result: Long): Long = {
    if(n < 1) result
    else fact(n-1, n * result)
  }

  fact(n, 1)
}

0 to 10 foreach { x => println(factorial(x)) }