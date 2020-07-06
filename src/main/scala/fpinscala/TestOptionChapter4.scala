package fpinscala

import scala.annotation.tailrec
import scala.collection.immutable.Nil

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }

  def orElse[B >: A](defaultOption: => Option[B]): Option[B] = {
    map(Some(_)) getOrElse defaultOption
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }
  }
}
case object None extends Option[Nothing]
case class Some[+A](value: A) extends Option[A]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap(aa => b map(bb => f(aa, bb)))
  }

  def map2UsingForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def sequence[A](xs: Seq[Option[A]]): Option[Seq[A]] = {
    xs match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map(l => hh +: l))
    }
  }

  def sequenceUsingTraverse[A](xs: Seq[Option[A]]): Option[Seq[A]] = traverse(xs)(x => x)

  def traverse[A, B](xs: Seq[A])(f: A => Option[B]): Option[Seq[B]] = {
    xs match {
      case Nil => Some(Nil)
      case h :: t => f(h) flatMap (hh => traverse(t)(f) map(bs => hh +: bs))
    }
  }

  def traverseUsingMap2[A, B](xs: Seq[A])(f: A => Option[B]): Option[Seq[B]] = {
    xs match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))((hh, tts) => hh +: tts)
    }
  }

  def traverseUsingFoldRight[A, B](xs: Seq[A])(f: A => Option[B]): Option[Seq[B]] = {
    xs.foldRight[Option[Seq[B]]](Some(Nil))((h, t) => map2(f(h), t)((hh: B, tts: Seq[B]) => hh +: tts))
  }
}

object TestOptionChapter4 {
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap {
      m => mean(xs.map(x => math.pow(x-m, 2)))
    }
  }

  def Try[A](f: => A): Option[A] = {
    try Some(f)
    catch {
      case e: Exception => None
    }
  }

  def parseInsuranceRateQuote(age: String, tickets: String): Option[Double] = {
    val optAge = Try { age.toInt }
    val optTickets = Try { tickets.toInt }

    Option.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, tickets: Int): Double = age * tickets

  def main(args: Array[String]): Unit = {
    val list = Seq(1.0, 2.0, 3.0, 4.0)
    println(mean(list).getOrElse(Double.NaN))
    println(mean(Seq.empty).getOrElse(Double.NaN))
    println(variance(list))
    println(variance(Seq.empty))

    val absLift: Option[Double] => Option[Double] = Option.lift(math.abs)
    println(absLift(Some(-3.0)))

    println(parseInsuranceRateQuote("12", "20"))
    println(parseInsuranceRateQuote("abc", "12"))

    val optionList1 = Seq(Some(1), Some(2), Some(3), Some(4))
    println(Option.sequence(optionList1))

    val optionList2 = Seq(Some(1), Some(2), None, Some(3), Some(4))
    println(Option.sequence(optionList2))

    println(Option.sequenceUsingTraverse(optionList1))
    println(Option.sequenceUsingTraverse(optionList2))

    val optionList3 = Seq("1", "2", "3", "4")
    println(Option.traverse(optionList3)(s => Try(s.toInt)))
    println(Option.traverse(optionList3)(s => Try(s.toInt)))
    println(Option.traverseUsingMap2(optionList3)(s => Try(s.toInt)))
    println(Option.traverseUsingFoldRight(optionList3)(s => Try(s.toInt)))

    println(Option.map2UsingForComprehension(Some(1), Some(2))(_ + _))
    println(Option.map2UsingForComprehension(Some(1), None)((x: Int, y: Int) => x+y))
  }
}
