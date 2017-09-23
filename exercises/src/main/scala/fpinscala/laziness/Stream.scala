package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeU(n: Int): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(_, _) if n == 0 => None
    case Cons(h, t) => Some(h(), t().takeU(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (p(h())) Cons(h, () => t() takeWhile p) else this
  }

  def takeWhileU(p: A => Boolean): Stream[A] = unfold(this) {
    case Empty => None
    case Cons(h, t) if p(h()) => Some(h(), t())
    case Cons(_, _) => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b /*empty*/)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons[B](f(h), t))

  def mapU[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def zipWith[B, C](b: => Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, b)) {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)){
    case (Empty, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
  }

  def startsWith[B](s: Stream[B]): Boolean = ???

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(_, t) => Some(t(), t())
    case Empty => None
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val onesU: Stream[Int] = unfold(1)(s => Some(s, s))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fromU(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibs(prev: Int, preprev: Int): Stream[Int] =
    cons(prev + preprev, fibs(prev + preprev, prev))

  def fibsU(): Stream[Int] = unfold(0, 1) { case (s) => Some(s._1 + s._2, (s._1 + s._2, s._1)) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(t => cons(t._1, unfold(t._2)(f))).getOrElse(empty)

  def main(args: Array[String]): Unit = {
    println(ones.take(5).tails.map(_.toList) toList)

  }
}