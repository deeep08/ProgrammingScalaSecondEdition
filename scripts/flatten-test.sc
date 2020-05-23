def flatten(list: List[Any]): List[Any] = {
  list flatMap {
    case l: List[_] => flatten(l)
    case e => List(e)
  }
}

println(flatten(List(
  List(1, 1),
  2,
  List(3,
       List(5, 8)))))

// P08

def compress[T](list: List[T]): List[T] = {
  def _compress(list: List[T], prev: T): List[T] = {
    list match {
      case head :: tail => if (head != prev) head :: _compress(tail, head) else _compress(tail, head)
      case Nil => List.empty[T]
    }
  }

  val res = _compress(list.tail, list.head)
  list.head :: res

  list.dropWhile()
}

def compressFor[T](list: List[T]): List[T] = {
  var result = List.empty[T]
  var prev = list.head
  result = prev :: result

  var i = list.tail
  while(i != Nil) {
    if(i.head != prev) {
      result = result :+ i.head
    }

    prev = i.head
    i = i.tail
  }

  result
}

println(compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
println(compressFor(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))