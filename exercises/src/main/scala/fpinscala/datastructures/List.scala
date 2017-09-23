package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalStateException()
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new IllegalStateException()
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => throw new IllegalStateException()
    case Cons(_, xs) => n match {
      case 1 => xs
      case a => drop(xs, a - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => throw new IllegalStateException()
    case Cons(head, xs) => if (f(head)) dropWhile(xs, f) else Cons(head, xs)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalStateException()
    case Cons(_, Nil) => Nil
    case Cons(h, xs) => Cons(h, init(xs))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((a, z) => Cons(a, z))

  def appendF[A](left: List[A], right: List[A]): List[A] =
    foldRight(left, right)((a, acc) => Cons(a, acc))

  def flatten[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])((acc, innerList) => append(acc, innerList))

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, z) => Cons(a + 1, z))

  def stringify(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, z) => Cons(a.toString, z))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, z) => Cons(f(a), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((a, z) => if (f(a)) Cons(a, z) else z)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map[A, List[B]](as)(a => f(a)))

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  /*def zipWith[A, B](l: List[A], r: List[A], f: (A, A) => B) = {
    var i = 0
    val indexed = map(l)(a => (a, {
      i = i + 1;
      i
    }))

    def get(i: Int) = {
      drop(r, i) match {
        case Nil => 0
        case Cons(x, _) => x
      }
    }

    map(indexed)(a => f(a._1, get(a._2)))
  }*/

 /* def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    zipWith(sup, sub, _ == _)

    def comp(ls: List[A]) =
      ls match {
        case Cons(x, xs) => Cons(x == head(sub), zipWith(xs, tail(sub), _ == _))
      }
  }*/
}
