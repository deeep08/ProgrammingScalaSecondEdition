package progscala2.concurrency

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random

object FutureExample {
  def main(args: Array[String]): Unit = {
    val futures = 1 to 9 map { i => Future {
        Thread.sleep(Random.nextLong(i * 1000))
        print(i)
        s"$i"
      }
    }

    println("Before foldLeft...")

    val res = Future.foldLeft(futures)("")(_ + _)

    println("Before await...")

    val value = Await.result(res, Duration.Inf)

    println

    println("After await...")

    println(value)

  }

}
