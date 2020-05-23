import scala.annotation.tailrec

def reduceLeft[A, B](in: Seq[A])(f: A => B): Seq[B] = {
  @tailrec
  def rl(acc: Seq[B], in: Seq[A]): Seq[B] = {
    in match {
      case head +: tail => rl(f(head) +: acc, tail)
      case Nil => acc
    }
  }
  rl(Seq.empty[B], in)
}

def reduceRight[A, B](in: Seq[A])(f: A => B): Seq[B] = {
  in match {
    case head +: tail => f(head) +: reduceRight(tail)(f)
    case _ => Seq.empty[B]
  }
}

reduceLeft(Seq(1,2,3,4,5)){ x => x*2 } foreach println
println
reduceRight(Seq(1,2,3,4,5)){ x => x*2} foreach println