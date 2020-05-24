import java.util.concurrent.Executors

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

println("Started promise example")

def doSomething(x: Int, message: String) = {
  Thread.sleep(x * 100)
  println(s"Message: $message, value: $x")
  x*2
}

val p = Promise[Int]
val f = p.future

val producer = Future {
  val r = doSomething(10, "Producer is computing")
  p success r
  doSomething(50, "Producer after computation")
}

val consumer = Future {
  var c = doSomething(1, "Consumer before consuming")
  f foreach {
    r => {
      println(s"Computed value: ${r * c}")
      c = r*c
    }
  }
  c
}

producer flatMap(self => Future {
  self * 2
}) foreach(x => println("Producer: " + x))

consumer map(self => self * 2) foreach(x => println("Consumer: " + x))

Await.result(producer, Duration.Inf)
Await.result(consumer, Duration.Inf)

println("Finished promise example")