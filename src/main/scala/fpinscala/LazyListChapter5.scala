package fpinscala

import scala.collection.immutable.List
import scala.collection.immutable.Nil
import util.{Stream => Stream}

package util {

  import scala.annotation.tailrec

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

    def mapUsingUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

    def takeUsingUnfold(n: Int): Stream[A] = Stream.unfold((n, this)) {
      case (x, Cons(h, t)) if x > 0 => Some((h(), (x-1, t())))
      case _ => None
    }

    def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
      Stream.unfold((this, bs)) {
        case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
        case _ => None
      }
    }

    def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
      Stream.unfold((this, bs)) {
        case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
        case (_, Cons(bh, bt)) => Some(((None, Some(bh())), (Empty, bt())))
        case (Cons(ah, at), _) => Some(((Some(ah()), None), (at(), Empty)))
        case _ => None
      }
    }

    def startsWith[B >: A](bs: Stream[B]): Boolean = {
      (this, bs) match {
        case (Cons(ah, at), Cons(bh, bt)) if ah() == bh() => at() startsWith bt()
        case (_, Empty) => true
        case _ => false
      }
    }

    def hasSubsequence[B >: A](bs: Stream[B]): Boolean = this match {
      case Empty => bs == Empty
      case _ if this startsWith bs => true
      case Cons(_, t) => t() hasSubsequence bs
    }

    def tails: Stream[Stream[A]] = Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    }

    def hasSubsequenceUsingTails[B >: A](bs: Stream[B]): Boolean = this.tails exists(_ startsWith bs)

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
      def scanRightHelper(z: B, as: Stream[A]): (B, Stream[B]) = {
        as match {
          case Cons(h, t) =>
            lazy val tuple = scanRightHelper(z, t())
            lazy val seed = f(h(), tuple._1)
            (seed, Stream.cons(seed, tuple._2))
          case Empty => (z, Stream(z))
        }
      }

      scanRightHelper(z, this)._2
    }

    def scanRightUsingFoldRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
      foldRight((z, Stream(z))) {(a, b) => {
        lazy val tuple = b
        lazy val seed = f(a, tuple._1)
        (seed, Stream.cons(seed, tuple._2))
      }}._2
    }

    override def toString: String = this.toList.toString()
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

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    def fibs: Stream[Int] = {
      def go(f: Int, s: Int): Stream[Int] = cons(f, go(s, f+s))
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, b)) => Stream.cons(a, unfold(b)(f))
        case None => Stream.empty[A]
      }
    }

    def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def fromUsingUnfold(s: Int): Stream[Int] = unfold(s)(x => Some((x, x+1)))

    def fibsUsingUnfold: Stream[Int] = unfold((0, 1))(x => Some((x._1, (x._2, x._1+x._2))))
  }
}

object LazyListChapter5 {
  val ones: Stream[Int] = Stream.cons(1, ones)

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

    println(ones.take(10).toList)

    println(Stream.constant(5).take(4).toList)
    println(Stream.constantUsingUnfold(5).take(4))

    println(Stream.from(6).take(4).toList)
    println(Stream.fromUsingUnfold(6).take(4))

    println(Stream.fibs.take(10))
    println(Stream.fibsUsingUnfold.take(10))

    println(Stream.unfold((0, 1))(x => Some(x._1, (x._2, x._1 + x._2))).take(10))

    println(nums.mapUsingUnfold(_ * 2).take(3))

    println(nums.takeUsingUnfold(3))

    println(nums.takeWhileUsingUnfold(_ < 3))

    println(nums.zipWith(Stream(1,1,1,1,1))(_ + _))

    println(nums.zipAll(Stream(1,1,1,1,1)))

    println(nums startsWith Stream(2,3))

    println(nums hasSubsequence Stream(2,3,4))

    println(nums.tails)

    println(nums hasSubsequenceUsingTails Stream(2,3,4))
    println(nums hasSubsequenceUsingTails Stream(2,3,4,6))

    println(Stream(1,2,3).scanRight(0)(_ + _))
    println(Stream(1,2,3).scanRightUsingFoldRight(0)(_ + _))

  }
}
