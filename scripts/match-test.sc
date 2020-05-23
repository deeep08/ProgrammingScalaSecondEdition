import java.io.FileNotFoundException

import scala.collection.{Factory, IterableFactory}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

def testMatch(x: Int) = {
  x match {
    case x if 1 to 10 contains x =>  " 1-10"
    case x if 11 to 20 contains x => "11-20"
    case x if 21 to 30 contains x => "21-30"
    case _ => "Out of range"
  }
}

def testMatch(x: String) = {
  x match {
    case s if s startsWith "o" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case _ => -1
  }
}

def testMatch(x: String, y: Int, z: Int) = {
  z match {
    case `y` => x
    case 1 | 2 => "one or two"
    case 3 => "three"
  }
}

def readFile(filename: String) = {
  var file: Option[Source] = None
  try {
    file = Some(Source.fromFile(filename))
    // for(line <- file.get.getLines) println(line)
    file.get.getLines foreach println
    throw new Exception("custom")
  } catch {
    case ex: FileNotFoundException => println(s"Exception while processing: $ex")
    case ex: Exception => println(s"Exception while processing: $ex")
  } finally {
    for (f <- file) f.close()
  }

}

1 to 50 foreach(x => println(testMatch(x)))
println(testMatch("one"))
println(testMatch("four", 1, 3))

readFile("../src/main/scala/progscala2/introscala/TryCatch.scala")

val a = ListBuffer.apply(1,2,3)
println(a.apply(0))