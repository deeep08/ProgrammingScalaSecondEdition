package progscala2.sorting

import scala.util.Sorting

private case class Person(name: String, age: Int) extends Ordered[Person] {
  override def compare(that: Person): Int = this.name compare that.name
}

object TestScalaSorting {

  def main(args: Array[String]): Unit = {
    val list = List(5,4,3,2,1,-1,-2,-3)
    println(list.sortWith((x, y) => math.abs(x) < math.abs(y)))
    println(list)

    val persons = List(Person("Harry", 10), Person("Bob", 20), Person("Joe", 5), Person("Dodo", 10))
    println(persons)
    println(persons.sorted)
    println(persons.sortBy(_.name))

    println(persons.sortBy(p => (p.age, p.name)))

    val array = Array("b", "d", "e", "a");
    Sorting.quickSort(array)
    println(array.mkString(" "))

    val personArray = Array(Person("Harry", 10), Person("Bob", 20), Person("Joe", 5), Person("Dodo", 10))
    Sorting.quickSort(personArray)(Ordering.by(_.name))
    println(personArray.mkString(" "))

    Sorting.quickSort(personArray)(Ordering[(Int, String)].on(p => (p.age, p.name)))
    println(personArray.mkString(" "))

  }

}
