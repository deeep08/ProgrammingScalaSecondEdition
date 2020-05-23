val bools = Seq(true, false);

for (bool <- bools) {
  bool match {
    case true => println("It is true")
    case false => println("It is false")
  }

}

for {
  bool <- bools
  if bool
} println(bool)

for {
  bool <- bools
  which = if (bool) "Got head" else "Got tail"
} Console println which

for(bool <- bools) {
  if(bool) {
    println("Got head")
  } else {
    println("Got tail")
  }
}