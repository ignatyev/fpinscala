package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A], d: Int = 0): Int = t match {
    case Leaf(_) => d
    case Branch(l, r) => depth(l, d + 1) max depth(r, d + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(1), Branch(Leaf(6), Leaf(11))), Leaf(2))
    println(map(tree)(a => a.toString))
    println(fold[Int, Tree[String]](tree)(a => Leaf(a.toString))((b1, b2) => Branch(b1, b2)))
  }
}