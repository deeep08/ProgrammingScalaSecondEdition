package fpinscala

import scala.collection.immutable.List
import scala.collection.immutable.Nil
import util.{Stream => Stream}

package util {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = {
      (n, this) match {
        case (x, Cons(h, t)) if x > 0 => Stream.cons(h(), t().take(x-1))
        case _ => Stream.empty
      }
    }

    def drop(n: Int): Stream[A] = {
      (n, this) match {
        case (x, Cons(_, t)) if x > 0 => t().drop(x-1)
        case (_, t) => t
      }
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
        case _ => Stream.empty
      }
    }

    def exists(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)

    def foldRight[B](z: B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def takeUsingFR(n: Int): Stream[A] =
      foldRight(Stream.empty[A])((a, b) => if(n == 0) Stream.empty[A] else Stream.cons(a, b.takeUsingFR(n-1)))

    def takeWhileUsingFR(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((a, b) => if(p(a)) Stream.cons(a, b) else Stream.empty)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def headOptionUsingFR(): Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if(p(h)) Stream.cons(h, t) else t)

    def append[B >: A](bs: => Stream[B]): Stream[B] = {
      foldRight(bs)((h, t) => Stream.cons(h, t))
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a) append b)
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, ts: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = ts
      Cons(() => head, () => tail)
    }

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) Empty
      else cons(as.head, apply(as.tail: _*))
    }

  }
}

object LazyListChapter5 {
  def main(args: Array[String]): Unit = {
    val nums = Stream(1, 2, 3, 4)
    println(nums.headOption)

    println(nums.toList)

    println(nums.take(2).toList)
    println(nums.takeUsingFR(2).toList)

    println(nums.drop(2).toList)

    println(nums.takeWhile(_ < 3).toList)
    println(nums.takeWhileUsingFR(_ < 3).toList)

    println(nums.exists(_ > 2))

    println(nums.forAll(_ > 0))

    println(nums.headOptionUsingFR())

    println(Stream.empty[Int].headOptionUsingFR())

    println(nums.map(x => { println(x); x*2 }).take(2).toList)

    println(List(1,2,3,4).map(x => { println(x); x*2 }).take(2))

    println(nums.filter(_ % 2 == 0).toList)

    println(nums.append(Stream(5)).toList)

    println(nums.flatMap(x => Stream(x, x*2, x*3)).toList)
  }
}
