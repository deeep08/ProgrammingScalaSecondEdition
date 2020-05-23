package progscala2.traits.ui

trait VetoableClicks extends Clickable {
  println("VetoableClicks start")
  val maxClicks = 1
  var clickCount = 0

  override def click(): Unit = {
    println("VetoableClicks start...")
    if(clickCount < maxClicks) {
      clickCount += 1
      super.click()
    }
    println("VetoableClicks end...")
  }
  println("VetoableClicks start")
}
