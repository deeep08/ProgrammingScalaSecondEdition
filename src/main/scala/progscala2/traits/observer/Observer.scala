package progscala2.traits.observer

trait Observer[-State] {
  def receiveUpdate(state: State): Unit
}
