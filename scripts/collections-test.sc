import scala.collection.mutable.ArrayBuffer

val arr = Array(1,2,3)
arr.update(1, 1)
arr(0) = 5
println(arr(0))
arr foreach(x => println(x))

var ab = new ArrayBuffer[Int]()

for (i <- 1 until 5) {
  ab = ab :+ i
}

println(ab.size)

ab foreach(x => println(x))

val list = List(1,2,3,1,2,3)
list :+ 4 foreach(x => println(x))
4 :: list foreach(x => println(x))
list :++ Array(5,6,7) foreach(x => println(x))
list foreach(x => println(x))
list map(x => x + 2)
list.distinct foreach(x => println(x))

println(list(0))

val t:Any = 1
t.asInstanceOf[Int]

var stateCapitals = Map("MP" -> "Bhopal", "MH" -> "Mumbai", "JK" -> "Srinagar")
println(stateCapitals.getOrElse("MP", "No capital found"))

stateCapitals = stateCapitals + ("TN" -> "Chennai")

stateCapitals foreach(kv => println(kv._1 + "->" + kv._2))

var lengths = stateCapitals map {
  kv => (kv._1, kv._2.length)
}

"abc".capitalize

var capitals = stateCapitals map {
  case (key, value) => (key, value.toUpperCase)
}

println(capitals)

println(s"Length of TN" + lengths.getOrElse("TN", 0))
println(stateCapitals.getOrElse("TN", "None"))

class Person(f: String, l: String) {
  var firstname = f
  var lastname = l

  def getName() = firstname + " " + lastname
}

val set = Set("a", "b", "b", "c")
println(set.size)

val l = List(1,2,3)
l.map[Int](_ * 2)

println("abc".capitalize)
println(Predef.augmentString("abc").capitalize)



