package progscala2.traits.ui

import progscala2.traits.observer.Observer

class ButtonObserver extends Observer[Clickable] {
  var count = 0

  override def receiveUpdate(state: Clickable) = {
    count += 1
  }
}
