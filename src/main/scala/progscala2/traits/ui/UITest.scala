package progscala2.traits.ui

object UITest {
  def main(args: Array[String]): Unit = {
    class TestButton extends Button("Submit") with ObservableClicks with VetoableClicks {
      println("TestButton start")
      println("TestButton end")
    }

    new TestButton

    println("------------------------")

    val obs1 = new ButtonObserver
    val obs2 = new ButtonObserver

    val button = new Button("Submit") with ObservableClicks with VetoableClicks
    println("------------------------")
    button.addObservers(obs1)
    button.addObservers(obs2)

    button.click()
//    button.click()

    assert(obs1.count == 1)
    assert(obs2.count == 1)

    println("------------------------")

    val button2 = new Button("Submit") with VetoableClicks with ObservableClicks
    println("------------------------")
    button2.addObservers(obs1)
    button2.addObservers(obs2)

    button2.click()
//    button2.click()

    assert(obs1.count == 2)
    assert(obs2.count == 2)
  }
}
