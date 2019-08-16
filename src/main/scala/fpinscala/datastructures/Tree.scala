package fpinscala.datastructures

import scala.util.matching.Regex.Match

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](as: Tree[A]): Int =
    as match {
      case Leaf(a) => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

  def max[A](as: Tree[Int]): Int =
    as match {
      case Leaf(a) => a
      case Branch(left, right) => max(left) max max(right)
    }

  def depth[A](as: Tree[A]): Int =
    as match {
      case Leaf(a) => 1
      case Branch(left, right) => depth(left) max depth(right) + 1
    }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] =
    as match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](as: Tree[A])(z: A => B)(f: (B, B) => B): B =
    as match {
      case Leaf(a) => z(a)
      case Branch(left, right) => f(fold(left)(z)(f), fold(right)(z)(f))
    }

}