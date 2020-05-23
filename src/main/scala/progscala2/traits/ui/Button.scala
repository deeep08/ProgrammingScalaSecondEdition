package progscala2.traits.ui

class Button(var name: String) extends Widget with Clickable {
  println("Button start")
  override protected def updateUI(): Unit = {
    println(s"Button: $name was clicked!!!")
  }
  println("Button end")
}

object Button {
  def apply(name: String) = new Button(name)
}
