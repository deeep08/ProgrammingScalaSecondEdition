import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

case class ThatsOdd(i: Int) extends RuntimeException(s"$i is Odd")

val doComplete: PartialFunction[Try[String], Unit] = {
  case Success(value) => println(value)
  case Failure(exception) => println(exception)
}

val futures = 0 to 9 map { i => if(i%2 == 0) Future.successful(i.toString) else Future.failed(ThatsOdd(i)) }

futures map {_ onComplete doComplete}