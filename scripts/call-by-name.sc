def count(condition: => Boolean)(body: Int => Unit): Unit = {
  if (condition) {
    body(1)
    count(condition)(body)
  }
}

var c = 0
count(c < 5) {
  r => {
    println(r)
    c += 1
  }
}