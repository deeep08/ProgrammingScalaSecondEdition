class CSuper {
  def msuper() = { println("CSuper") }
}

class C extends CSuper {
  def m() = { println("C") }
}

class CSub extends C {
  def msub() = { println("CSub") }
}

var f: C => C = (c: C) => { c.msuper(); c.m();  new C }

f(new C)

f = (c: CSuper) => { c.msuper(); new CSub }

f(new CSub)

f = (c: C) => { c.m(); new CSub }