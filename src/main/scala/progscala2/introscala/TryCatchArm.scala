package progscala2.introscala

import scala.io.Source
import scala.util.control.NonFatal
import scala.language.reflectiveCalls

object manage {
  def apply[R <: { def close():Unit }, T](resource: => R)(f: R => T): Unit = {
    var res: Option[R] = None
    try {
      res = Some(resource)
      f(res.get)
    } catch {
      case NonFatal(ex) => println(s"Error while processing: $ex")
    } finally {
      for(r <- res) {
        println("file closed")
        r.close()
      }
    }
  }
}

object TryCatchArm {
  def main(args: Array[String]): Unit = {
    args foreach countLines
  }

  def countLines(filename: String) = {
    manage(Source.fromFile(filename)) {
      source => println(s"The file $filename contains ${source.getLines.size} lines")
    }
  }
}
