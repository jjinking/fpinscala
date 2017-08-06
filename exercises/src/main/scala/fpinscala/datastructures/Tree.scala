package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def numNodes(t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => numNodes(l) + numNodes(r) + 1
  }

  def maxElement(t: Tree[Int]): Int = t match {
    case Leaf(val) => val
    case Branch(l, r) => maxElement(l) max maxElement(r)
  }

  def maxDepth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => maxDepth(l) max maxDepth(r) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(val) => Leaf(f(val))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A], fz: A => B)(combine: (B, B) => B): B = t match {
    case Leaf(a) => fz(a)
    case Branch(l, r) => combine(fold(l, fz)(combine), fold(r, fz)(combine))
  }

  def sizeViaFold(t: Tree[A]): Int = fold(t, a => 1)((b1, b2) => b1 + b2 + 1)

  def maximumViaFold(t: Tree[A]): Int = fold(t, a=>a)(_ max _)

  def depthViaFold(t: Tree[A]): Int = fold(t, a=>0)(_ max _ + 1)

  def mapViaFold(t: Tree[A])(f: A => B): Tree[B] = fold(t, a=>Leaf(f(a)): Tree[B])(Branch(_, _))
}
