var seqs = Seq(1, 2, 3.7, "one", "two", 'four)

for(s <- seqs) {
  val str = s match {
    case 1 => "value = 1"
    case _: Int | Double =>  s"value = $s"
    case "one" => "value = one"
    case _: String => s"value = $s"
    case _ => s"unknown value = $s"
  }

  println(str)
}

def checkY(y: Int) = {
  for(x <- Seq(100,101,102)) {
    x match {
      case `y` => println("Found y")
      case i: Int => println("found " + i)
    }
  }
}

checkY(100)