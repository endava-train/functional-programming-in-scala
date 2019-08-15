package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object LinkedList {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](t: A, as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(t, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => if (xs == Nil) Nil else Cons(x, init(xs))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y)=> x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def reverse[A](ns: List[A], ap: List[A]): List[A] =
    ns match {
      case Nil => Nil
      case Cons(x, Nil) => Cons(x, ap)
      case Cons(x, xs) => reverse(xs, Cons(x, ap))
    }

  def reverse[A](ns: List[A]): List[A] = {
    @annotation.tailrec
    def go(as: List[A], current: List[A]): List[A] =
      as match {
        case Nil => current
        case Cons(x, xs) => go(xs, Cons(x, current))
      }

    ns match {
      case Nil => Nil
      case Cons(x, xs) => go(xs, Cons(x, Nil))
    }
  }

  def reverse2[A](ns: List[A]): List[A] = {
    ns match {
      case Nil => Nil
      case Cons(x, xs) => foldLeft(xs, Cons(x, Nil))((a, b) => Cons(b, a))
    }
  }

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  def appendViaFoldRight[A](as: List[A], element: A): List[A] =
    foldRight(as, Cons(element, Nil))((a, b) => Cons(a, b))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def concatList[A](as: List[List[A]]): List[A] = {
    val answer = as match {
      case Nil => Nil
      case Cons(x, xs) => foldLeft(xs, reverse(x))((b, a) => append(reverse(a), b))
    }
    reverse(answer)
  }

  def addOne(as: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }

}
