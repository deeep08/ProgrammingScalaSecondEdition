import scala.annotation.tailrec

def last[A](list: Seq[A]): A = list match {
  case head :: Nil => head
  case _ :: tail => last(tail)
  case _ => throw new NoSuchElementException
}

def penultimate[A](list: List[A]): Option[A] = {
  list match {
    case _ :: Nil => None
    case head :: _ :: Nil => Option(head)
    case _ :: _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }
}

def nthFromLast[A](n: Int, list: List[A]): Option[A] = {
  @tailrec
  def nthLastR[A](count: Int, res: List[A], list: List[A]): Option[A] = {
    list match {
      case Nil if count > 0 => None
      case Nil => Option(res.head)
      case _ :: tail => nthLastR(count - 1, if(count > 0) res else res.tail, tail)
    }
  }

  if(n <= 0) None else nthLastR(n, list, list)
}

def nth[A](k: Int, list: List[A]): Option[A] = {
  list match {
    case _ if k < 0 => None
    case Nil if k > 0 => None
    case head :: tail => if (k == 0) Option(head) else nth(k - 1, tail)
  }
}

def length[A](list: List[A]): Int = {
  @tailrec
  def len[A](count: Int, ls: List[A]): Int = {
    ls match {
      case Nil => count
      case _ :: tail => len(count+1, tail)
    }
  }

  len(0, list)
}

def reverse[A](list: List[A]): List[A] = {
  def rev[A](res: List[A], ls: List[A]): List[A] = {
    ls match {
      case head :: tail => rev(head :: res, tail)
      case Nil => res
    }
  }

  rev(List.empty[A], list)
}

println(last(List(1,2,3,4,5,9)))
println(penultimate(List(1,2,3,4,5,9)))
println(nthFromLast(1, List(1,2,3,4,5,6)).getOrElse(-1))
println(nth(5, List(1,2,3,4,5,6)).getOrElse(-1))
println(length(List.empty[Int]))

println(reverse(List(1,2,3,4,5)))