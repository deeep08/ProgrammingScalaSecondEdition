abstract class AbsIterator {
  type T
  def hasNext(): Boolean
  def next(): T
}

class StringIterator(var str: String) extends AbsIterator {
  type T = Char

  private var count = 0

  override def hasNext(): Boolean = count < str.length

  override def next(): Char = {
    val ch = str.charAt(count)
    count = count + 1
    ch
  }
}

trait RichIterator extends AbsIterator {
  def foreach(f: T => Unit): Unit = {
    while(hasNext()) {
      f(next())
    }
  }
}

class RichStringIterator(str: String) extends StringIterator(str) with RichIterator

val sitr = new StringIterator("Scala")
while(sitr.hasNext()) print(sitr.next())

val rsitr = new RichStringIterator("Scala")
rsitr.foreach {
  c => println(c.toUpper)
}