val seq = Seq(1,2,3,4,5)
val emptySeq = Seq.empty[Int]
val list = List(1,2,3,4,5)
val emptyList = Nil
val map = Map("one" -> 1, "two" -> 2, "three" -> 3)
val emptyMap = Map.empty[String, Int]

val allSeq = Seq(seq, emptySeq, list, emptyList, map.toSeq, emptyMap.toSeq)

def printSeq[T](seq: Seq[T]): String = {
  seq match {
    case head +: tail => s"$head +: " + printSeq(tail)
    case Nil => "Nil"
  }
}

allSeq foreach(x => println(printSeq(x)))
println("--------------------")
for(seq <- allSeq) {
  println(printSeq(seq))
}

def printList[T](list: List[T]): String = {
  list match {
    case head :: tail => s"$head -> " + printList(tail)
    case Nil => "Nil"
  }
}

println("--------------------")
val listSeq = Seq(list, emptyList)
for(list <- listSeq) {
  println(printList(list))
}

