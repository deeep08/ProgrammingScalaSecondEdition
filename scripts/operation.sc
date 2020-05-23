abstract class Operation {
  def operate(x: Int): Int = x
}

trait Doubling extends Operation {
  override def operate(x: Int): Int = super.operate(x * 2)
}

trait Incrementing extends Operation {
  override def operate(x: Int): Int = super.operate(x+1)
}

class Double extends Operation with Doubling
class Increment extends Operation with Incrementing
class DoubleThenIncrement extends Operation with Incrementing with Doubling
class IncrementThenDouble extends Operation with Doubling with Incrementing

val a = new Double
println(a.operate(5)) // 10

val b = new Increment
println(b.operate(5)) // 6

val c = new DoubleThenIncrement
println(c.operate(5)) // 11

val d = new IncrementThenDouble
println(d.operate(5)) // 12

println("------------")
val list = List(1,5,3,7,2)

println(list.filter(_%2 == 0).minByOption(identity))