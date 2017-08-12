package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(val) => Some(f(val))
    case _ => _
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(val) => val
    case _ => default
  }

  /**
    * Some(a)
    * map(f) ->
    * Some(Some(b)) getOrElse None  -> Some(b)
    * Some(None) -> None
    * 
    * None(a)
    * map(f) ->
    * None getOrElse None  -> None
    */
  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = this map {val => Some(val)} getOrElse ob

  /**
    *  this: Some(a)
    *  this map f ->
    *    Some(true)
    *    Some(false)
    *
    *  this: None
    *  this map f ->
    *    None
    */
  def filter(f: A => Boolean): Option[A] = if (this map f getOrElse false) this else None
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(math.pow(_ - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
    // a match {
    //   case Some(aVal) => b match {
    //     case Some(bVal) => Some(f(a, b))
    //     case None => None
    //   }
    //   case None => None
    // }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    // a foldRight Some(List()) {(optA: Option[A], agg: Option[List[A]]) =>
    //   map2(optA, agg)(_::_)
    // }
    a match {
      case Nil => Some(Nil)
      case h:Option[A] :: t:List[Option[A]] => h flatMap (hh => sequence(t) map ((tt) => hh :: tt))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_::_)

  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(b => b)
}
