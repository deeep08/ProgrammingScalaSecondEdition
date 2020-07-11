package fpinscala

trait RNG {
  def nextInt: (Int, RNG)

  def positiveInt: (Int, RNG)
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
}

object FunctionalStateChapter6 {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val (a, rng2) = rng.nextInt
    val (b, rng3) = rng2.nextInt
    val (c, rng4) = rng.nextInt

    println(s"$a $b $c")

    println(rng2.positiveInt._1)
    println(rng3.positiveInt._1)
    println(rng4.positiveInt._1)
  }
}
