package progscala2.traits.observer

trait Observable[Subject] {
  private var observers: List[Observer[Subject]] = List.empty

  def addObservers(observer: Observer[Subject]): Unit = observers ::= observer

  def notifyObservers(subject: Subject): Unit = {
    observers foreach (_.receiveUpdate(subject))
  }
}
