package progscala2.introscala

import scala.io.Source
import scala.util.control.NonFatal

object TryCatch {
  def main(args: Array[String]) = {
    val arr = Array(1,2,3)
    arr.update(0, 5)

    //args foreach countLines
  }

  def countLines(filename: String) = {
    var file: Option[Source] = None
    try {
      file = Some(Source.fromFile(filename))
      val lines = file.get.getLines

      val upcasedLines = for {
        line <- lines
        if (line contains "import") || (line contains "So")
        upcasedLine = line.toUpperCase
      } yield upcasedLine

      upcasedLines foreach println

      val lineCount = lines.size
      println(s"The file $filename contains $lineCount lines")
    } catch {
      case NonFatal(ex) => println(s"Error while reading file: $ex")
    } finally {
      for (f <- file) {
        f.close()
      }
    }
  }
}
