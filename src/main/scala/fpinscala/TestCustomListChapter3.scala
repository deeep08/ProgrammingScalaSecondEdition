package fpinscala

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, xs: List[A]) extends List[A]

object List {
  def sum(xs: List[Int]): Int = {
    xs match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(xs: List[Int]): Double = xs match {
    case Nil => 1.0
    case Cons(0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](h: A, as: List[A]): List[A] = {
    Cons(h, as)
  }

  def drop[A](n: Int, as: List[A]): List[A] = {
    (n, as) match {
      case (0, t) => t
      case (_, Nil) => Nil
      case (x, ts) => drop(x-1, List.tail(ts))
    }
  }

  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => if(p(h)) dropWhile(t)(p) else t
    }
  }

  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def reverseHelper(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => acc
        case Cons(h, t) => reverseHelper(t, Cons(h, acc))
      }
    }

    reverseHelper(as, Nil)
  }

  def init[A](as: List[A]): List[A] = {
    @tailrec
    def initHelper(list: List[A], acc: List[A]): List[A] = {
      list match {
        case Nil => Nil
        case Cons(_, Nil) => acc
        case Cons(h, tail) => initHelper(tail, Cons(h, acc))
      }
    }

    List.reverse(initHelper(as, Nil))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    val p: (A, B) => B = (x, y) => f(y, x)
    foldRight(reverse(as), z)(p)
  }

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    val p: (B, A) => B = (x, y) => f(y, x)
    foldLeft(reverse(as), z)(p)
  }

  def sumFR(as: List[Int]): Int = {
    foldRight(as, 0)(_ + _)
  }

  def productFR(as: List[Double]): Double = {
    foldRight(as, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, l) => l+1)
  }

  def append[A](as: List[A], bs: List[A]): List[A] = {
    as match {
      case Nil => bs
      case Cons(x, Nil) => Cons(x, bs)
      case Cons(x, xs) => Cons(x, append(xs, bs))
    }
  }

  def appendUsingFoldRight[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)((x, y) => Cons(x, y))
  }

  def appendUsingFoldLeft[A](as: List[A], bs: List[A]): List[A] = {
    foldLeft(reverse(as), bs)((x, y) => Cons(y, x))
  }

  def concat[A](xss: List[List[A]]): List[A] = {
    foldLeft(xss, Nil: List[A])((x: List[A], y: List[A]) => append(x, y))
  }

  def map[A, B](as: List[A])(f: A=>B): List[B] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(_, xs) => filter(xs)(f)
    }
  }

  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if(f(x)) List(x) else Nil)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    def flatMapHelper(as: List[A], bs: List[B]): List[B] = {
      bs match {
        case Nil => flatMap(as)(f)
        case Cons(y, Nil) => Cons(y, flatMap(as)(f))
        case Cons(y, ys) => Cons(y, flatMapHelper(as, ys))
      }
    }

    as match {
      case Nil => Nil
      case Cons(x, xs) => flatMapHelper(xs, f(x))
    }
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    (as, bs) match {
      case (Nil, Nil) | (Nil, Cons(_, _)) | (Cons(_, _), Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  def hasSubsequence[A](as: List[A], bs: List[A]): Boolean = {
    @tailrec
    def hasSubsequenceHelper[A](xs: List[A], ys: List[A]): Boolean = {
      (xs, ys) match {
        case (Cons(ah, at), Cons(bh, bt)) => if (ah == bh) hasSubsequenceHelper(at, bt) else hasSubsequenceHelper(at, bs)
        case (_, Nil) => true
        case (Nil, _) => false
      }
    }

    hasSubsequenceHelper(as, bs)
  }

  def main(args: Array[String]): Unit = {
    val list = List(0, 1, 2, 3, 4)

    println(List.sum(list))
    println(List.product(list))

    println(List.tail(list))
    println(setHead(-1, list))

    println(List.drop(2, list))
    println(List.dropWhile(list)(x => x<2))

    println(List.reverse(list))
    println(List.init(list))

    println(sumFR(list))
    println(productFR(List(1.0, 2.0, 3.0, 4.0)))

    println(foldRight(list, Nil: List[Int])(Cons(_, _)))
    println(foldRightUsingFoldLeft(list, Nil: List[Int])(Cons(_, _)))

    println(length(list))

    println(foldLeft(list, "")(_ + _))
    println(foldLeftUsingFoldRight(list, "")(_ + _))
    println(foldLeft(list, 0)((x, _) => x+1))

    println(foldLeft(list, Nil: List[Int])((x, y) => Cons(y, x)))

    println(append(list, List(5,6,7,8)))
    println(appendUsingFoldRight(list, List(5,6,7,8)))
    println(appendUsingFoldLeft(list, List(5,6,7,8)))

    println(concat(List(list, List(5,6), List(7,8), List(9))))

    println(map(list)(_ + 1))
    println(map(List(1.0, 2.0, 3.0))(_.toString + "->"))

    println(filter(list)(_ % 2 == 1))
    println(filterUsingFlatMap(list)(_ % 2 == 1))

    println(flatMap(list)(x => List(x, x)))

    println(zipWith(list, List(5,6,7,8))(_ + _))

    println(hasSubsequence(list, List(1,2,4)))
    println(hasSubsequence(list, List(1,2,3,4)))
    println(hasSubsequence(list, List(2,3,4,5)))
    println(hasSubsequence(list, List(3,4)))
    println(hasSubsequence(list, List(1)))
    println(hasSubsequence(list, Nil))
  }
}
