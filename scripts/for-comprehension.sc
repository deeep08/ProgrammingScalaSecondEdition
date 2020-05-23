import scala.language.postfixOps

for {i <- 0 to 10
     if i % 3 == 0
     } println(i)

val list = List("Harry", "Bob", "Bernard", "Joe", "Monica", "Ross")

println("---------------------")
for (name <- list)
  println(name)

println("---------------------")
for {
  name <- list
  if name startsWith "B"
  uc = name.toUpperCase
} println(uc)

println("---------------------")
var oNames = for {
  name <- list
  if name contains "o"
  uc = name prependedAll "Mr/Ms. "
} yield uc

oNames foreach println

println("---------------------")

var ran: Int = math.random() * 10 toInt
val filename = if(ran % 2 == 0) {
  "Filename is even"
} else {
  "Filename is odd"
}
Console println filename + " " + ran

println("---------------------")
for {
  i <- 1 to 10
} println(s"7 x $i = ${7*i}")