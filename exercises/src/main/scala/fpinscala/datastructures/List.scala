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
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0 ) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => _
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftSum[A](l: List[A]): Int = foldLeft(l, 0)(_+_)
  def foldLeftProd[A](l: List[A]): Int = foldLeft(l, 1)(_ * _)
  def foldLeftLen[A](l: List[A]): Int = foldLeft(l, 0)((r, _) => r + 1)

  def foldReverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  // foldRight via foldLeft
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(foldReverse(l), z)((b, a) => f(a, b))


  def foldRightAppend[A](l1: List[A], l2: List[B]) = foldRight(l1, l2)((x, r) => Cons(x, r))

  def concat[A](l: List[List[A]]) = foldRight(l, Nil:List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: list[B])((a, rL) => Cons(f(a), rL))

  // def filter[A](l: List[A], f: A => Boolean): List[A] = l match {
  //   case None => None
  //   case Cons(x, xs) if f(x) => Cons(x, filter(xs))
  //   case _ => filter(xs)
  // }
  def filter[A](l: List[A], f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, r) => if (f(a)) Cons(a, r) else r)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, rs) => append(f(a), rs))

  def filterViaFlatMap[A](l: List[A], f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def vectorAdd(l1: List[Int], l2: List[Int]): List[Int] = {
    l1.zip(l2).map(_+_)
  }
  
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup.slice(0, sup.length - sub.length + 1).zipWithIndex.map(_._2)  // Get the starting indices
      .foldRight(Nil: List[List[A]])((i, rs) => sup.slice(i, i + sub.length)::rs) // Get all subseqs
      .exists(ss => ss == sub)
  
}
