import scala.annotation.tailrec

def fibo(): LazyList[BigInt] = {
  BigInt(0) #:: BigInt(1) #:: fibo().zip(fibo().tail).map(t => t._1 + t._2)
}

fibo take 10 foreach(i => print(s"$i "))

println

lazy val fib: LazyList[Int] = {
  def loop(h: Int, n: Int): LazyList[Int] = h #:: loop(n, n+h)
  loop(0, 1)
}

var ll = LazyList(1,2,3)
var ll2 = 4 #:: ll

ll2 take 5 foreach(x => println(s"ll = $x"))

fib take 10 foreach println