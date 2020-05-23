package progscala2.traits.ui

import progscala2.traits.observer.Observable

trait ObservableClicks extends Clickable with Observable[Clickable] {
  println("ObservableClicks start...")

  override def click(): Unit = {
    println("ObservableClicks start...")
    super.click()
    notifyObservers(this)
    println("ObservableClicks end")
  }

  println("ObservableClicks end...")
}
