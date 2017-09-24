package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
    case Cons(hf, tf) => hf() :: tf().toList
  }

  def take(n: Int): Stream[A] = this match {
    if (n <= 0) empty
    else {
      case Empty => throw new Error
      case Cons(hf, tf) => cons(hf(), tf().take(n - 1))
    }
  }

  @annotation.tailrec
  def drop(n: Int): Stream[A] = this match {
    if (n <= 0) this
    else {
      case Empty => throw new Error
      case Cons(_, tf) => tf().drop(n - 1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[A, B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append(s: => Stream[A]): Stream[A] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[A, B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this)(s => s match {
                                        case Cons(h, t) => Some((f(h()), t()))
                                        case Empty => None
    })

  def takeViaUnfold(n: Int): Stream[A] = 
    Stream.unfold[A, (Int, Stream[A])]((n, this))(
      case (x, s) => s match {
        case Cons(h, t) if x > 0 => Some((h(), (x - 1, t())))
        case _ => None
      })

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold[A, Stream[A]](this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  // def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a,b) match {
  //   case (Nil, _) => Nil
  //   case (_, Nil) => Nil
  //   case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1, h2), zipWith(t1,t2)(f))
  // }
  def zipWithViaUnfold[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold[C, (Stream[A], Stream[B])]((this, that)) {
      _ match {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2)) {
      _ match {
        case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty[B])))
        case (Empty, Cons(h, t)) => Some(((None, Some(h())), (empty[A], t())))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2()), (t1(), t2()))))
        case (Empty, Empty) => None
      }
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll[A](s).takeWhile(case (_, opt2) => !opt2.isEmpty).forAll {case (x1, x2) => x1 == x2}
  //zipWithViaUnfold[A, Boolean](s)(_ == _).forAll(_)

  def tails: Stream[Stream[A]] =
    Stream.unfold[Stream[A], Stream[A]](this) { s =>
      case Cons(_, t) => Some((s, t()))
      case Empty => None
    } append Stream(empty[A])

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(z){(a: A, b: B) =>

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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def f(a: Int = 0, b: Int = 1): Stream[Int] =
      Stream.cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some(nextValue, nextState) => Streams.cons(nextValue, unfold(nextState)(f))
  }

  def fibsWithUnfold: Stream[Int] =
    unfold[Int, (Int, Int)]((0, 1))((a, b) => Some(a, (b, a + b)))

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold[Int, Int](n)(x => Some(x, x + 1))

  def constantWithUnfold(a: A): Stream[A] =
    unfold[A, A](a)(_ => Some((a, a)))

  def onesWithUnfold: Stream[Int] =
    unfold[Int, Int](1)(_ => Some((1, 1)))

}
