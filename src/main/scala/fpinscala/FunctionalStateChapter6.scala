package fpinscala

import scala.collection.immutable.{List => List}
import scala.collection.immutable.{Nil => Nil}

trait RNG {
  def nextInt: (Int, RNG)

  def positiveInt: (Int, RNG)

  def ints(count: Int): (List[Int], RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def positiveInt: (Int, RNG) = {
    val (n, rng) = nextInt
    if(n == Int.MinValue) (0, rng) else (math.abs(n), rng)
  }

  def ints(count: Int): (List[Int], RNG) = {
    def IntsHelper(n: Int, rng: RNG): (List[Int], RNG) = {
      if(n == 0)
        (Nil, rng)
      else {
        val (a, r1) = rng.nextInt
        val (as, r2)  = IntsHelper(n-1, r1)
        (a :: as, r2)
      }
    }

    IntsHelper(count, this)
  }

  def map[A, B](s: SimpleRNG.Rnd[A])(f: A => B): SimpleRNG.Rnd[B] = {
    rng => {
      val (a, r1) = s(rng)
      (f(a), r1)
    }
  }

  def doubleUsingMap(): (Double, RNG) = map(_.nextInt)(i => i/Int.MaxValue.toDouble)(this)

  def map2[A,B,C](s: SimpleRNG.Rnd[A], t: SimpleRNG.Rnd[B])(f: (A, B) => C): SimpleRNG.Rnd[C] = {
    rng => {
      val (a, r1) = s(rng)
      val (b, r2) = t(r1)
      (f(a, b), r2)
    }
  }

  def intDoubleUsingMap2(): ((Int, Double), RNG) = map2(_.nextInt, SimpleRNG.nextDouble)((_, _))(this)

  def sequence[A](fs: List[SimpleRNG.Rnd[A]]): SimpleRNG.Rnd[List[A]] = {
    fs match {
      case h :: t => map2(h, sequence(t))((x, xs) => x :: xs)
      case Nil => SimpleRNG.unit(List.empty[A])
    }
  }

  def sequenceUsingFR[A](fs: List[SimpleRNG.Rnd[A]]): SimpleRNG.Rnd[List[A]] = {
    fs.foldRight(SimpleRNG.unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  }

  def flatMap[A,B](fs: SimpleRNG.Rnd[A])(g: A => SimpleRNG.Rnd[B]): SimpleRNG.Rnd[B] = {
    rng => {
      val (a, r1) = fs(rng)
      g(a)(r1)
    }
  }

  def mapUsingFM[A,B](fs: SimpleRNG.Rnd[A])(g: A => B): SimpleRNG.Rnd[B] = {
    flatMap(fs)(x => SimpleRNG.unit(g(x)))
  }

  def map2UsingFM[A,B,C](fs: SimpleRNG.Rnd[A], gs: SimpleRNG.Rnd[B])(h: (A, B) => C): SimpleRNG.Rnd[C] = {
    flatMap(fs)(a => map(gs)(b => h(a, b)))
  }
}

object SimpleRNG {
  type Rnd[+A] = RNG => (A, RNG)

  def unit[A](value: A): Rnd[A] = RNG => (value, RNG)

  def nextDouble(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    (i.toDouble/Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (a, r1) = rng.nextInt
    val (b, r2) = nextDouble(r1)
    ((a, b), r2)
  }
}

object FunctionalStateChapter6 {
  import SimpleRNG.Rnd

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val (a, rng2) = rng.nextInt
    val (b, rng3) = rng2.nextInt
    val (c, rng4) = rng.nextInt

    println(s"$a $b $c")

    println(rng2.positiveInt._1)
    println(rng3.positiveInt._1)
    println(rng4.positiveInt._1)

    printf(SimpleRNG.nextDouble(rng3)._1.formatted("|%-10.5f|\n"))
    printf(SimpleRNG.nextDouble(rng4)._1.formatted("|%-10.5f|\n"))
    println(SimpleRNG.intDouble(rng4)._1)

    println(rng4.ints(5))

    type RndInt = Rnd[Int]

    val randomInt: RndInt = _.nextInt
    val positiveInt: RndInt = _.positiveInt
    val posIntPlusOne: RndInt = r => { val (a, r1) = r.positiveInt
      (a+1, r1)
    }

    println(randomInt(rng4))

    println(rng.sequence(List(randomInt, positiveInt, posIntPlusOne))(rng2))
    println(rng.sequenceUsingFR(List(randomInt, positiveInt, posIntPlusOne))(rng3))

    println(rng.mapUsingFM(_.positiveInt)(_/2)(rng))
  }
}
