package fpinscala

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Leaf[+A](value: A) extends Tree[A]
case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def traverse[A](root: Tree[A]): Unit = {
    root match {
      case Leaf(x) => print(x + " ")
      case Branch(x, b1, b2) => {
        print(x + " ")
        traverse(b1)
        traverse(b2)
      }
      case _ => // do nothing
    }
  }

  def size[A](root: Tree[A]): Int = {
    root match {
      case Leaf(_) => 1
      case Branch(_, b1, b2) => 1 + size(b1) + size(b2)
      case Empty => 0
    }
  }

  def maxInt(root: Tree[Int]): Int = {
    root match {
      case Leaf(x) => x
      case Branch(x, b1, b2) => x max maxInt(b1) max maxInt(b2)
      case Empty => 0
    }
  }

  def max[A](root: Tree[A])(ordered: (A, A) => Boolean): Option[A] = {
    def maxHelper(root: Tree[A], max: A): A = {
      root match {
        case Empty => max
        case Leaf(x) => if(ordered(x, max)) x else max
        case Branch(x, b1, b2) => {
          val newMax = if(ordered(x, max)) x else max
          val b1Max: A = maxHelper(b1, newMax)
          val b2Max: A = maxHelper(b2, newMax)
          if(ordered(b1Max, b2Max)) b1Max else b2Max
        }
      }
    }

    root match {
      case Empty => None
      case Branch(x, _, _) => Some(maxHelper(root, x))
      case Leaf(x) => Some(x)
    }
  }

  def depth[A](root: Tree[A]): Int = {
    root match {
      case Leaf(_) => 1
      case Branch(_, b1, b2) => 1 + (depth(b1) max depth(b2))
      case Empty => 0
    }
  }

  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = {
    root match {
      case Leaf(x) => Leaf(f(x))
      case Branch(x, b1, b2) => Branch(f(x), map(b1)(f), map(b2)(f))
      case Empty => Empty
    }
  }

  def fold[A, B](root: Tree[A], z: B)(f: A => B)(g: (A, B, B) => B): B = {
    root match {
      case Empty => z
      case Leaf(x) => f(x)
      case Branch(x, b1, b2) => g(x, fold(b1, z)(f)(g), fold(b2, z)(f)(g))
    }
  }

  def sizeUsingFold[A](root: Tree[A]): Int = {
    fold(root, 0)(_ => 1)((_, y, z) => 1+y+z)
  }

  def depthUsingFold[A](root: Tree[A]): Int = {
    fold(root, 0)(_ => 1)((_, y, z) => 1 + (y max z))
  }

  def mapUsingFold[A, B](root: Tree[A])(f: A=>B): Tree[B] = {
    fold(root, Empty: Tree[B])(x => Leaf(f(x)))((x, y, z) => Branch(f(x), y, z))
  }

  def main(args: Array[String]): Unit = {
    val b1 = Branch(5, Leaf(1), Empty)
    val b2 = Branch(15, Leaf(2), Leaf(4))
    val b3 = Branch(24, Empty, b2)

    val root = Branch(8, b1, b3);

    traverse(root)
    println
    println(size(root))

    val maxElement = max(root)((x, y) => x >= y) match {
      case Some(x) => x
    }

    println(maxElement)
    println(maxInt(root))
    println(max(Leaf("Deep"))((x, y) => x >= y))

    println(depth(root))

    traverse(map(root)(_ * 2))

    println
    println(sizeUsingFold(root))
    println(depthUsingFold(root))
    traverse(mapUsingFold(root)(_ * 2))
  }
}
