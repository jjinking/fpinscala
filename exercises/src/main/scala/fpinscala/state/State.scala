package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i1, rng2) = rng.nextInt
    if (i1 == Int.MinValue) (0, rng2)
    else (abs(i1), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)
    -(il.toDouble / Int.MinValue), rng2
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r2) = intDouble(rng)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    (d1, d2, d3), rng4
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count - 1)(r1)
      (i :: l, r2)
    }
  }

  def doubleViaMap: Rand[Double] =
    map[Int, Double](nonNegativeInt)(i => -(i.toDouble / Int.MinValue))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  // def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List()))((r: Rand[A], acc: Rand[List[A]]) =>
      map2(r, acc){(a, as) => a::as})

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rA) = f(rng)
    g(a)(rA)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap[Int, Int](nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    //flatMap(s)(a => unit(f(a)))
    flatMap(s)(a => rng => (f(a), rng))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    //flatMap(ra)(a => map(rb)(b => f(a, b)))
    flatmap(ra)(a => rng => {
                  val (b, rng2) = rb(rng)
                  (f(a, b), rng2)
                })

}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = s => {
    val (a, s2) = run(s)
    (f(a), s2)
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = s => {
    val (a, s1) = run(s)
    val (b, s2) = sb.run(s1)
    (f(a, b), s2)
  }
    
  def flatMap[B](f: A => State[S, B]): State[S, B] = s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[A](a: A): State[A] = State(s => (a, s))

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit(List()))((r: State[A], acc: State[List[A]]) =>
      acc.map2(r){(a, as) => a::as})

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  
}
