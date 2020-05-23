package progscala2.traits.ui

trait Clickable {
  println("Clickable start")
  protected def updateUI(): Unit

  def click():Unit = {
    println("Clickable start...")
    updateUI()
    println("Clickable end...")
  }
  println("Clickable end")
}
