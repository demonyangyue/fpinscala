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
    val (n, nextRNG) = rng.nextInt()
    (if (n < 0) -(n + 1) else n , nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble/(Int.MaxValue + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRNG) = rng.nextInt()
    val (j, nextNextRNG) = double(nextRNG)
    ((i, j), nextNextRNG)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (x, r1) = rng.nextInt()
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  def _double: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def doubleInt_2(rng: RNG): Rand[(Double, Int)] = {
    map2(int, int)(_.toDouble/(Int.MaxValue + 1), int)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = ra(rng1)
      (f(a, b), rng2)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (x, rng1) = f(rng)
      g(x)(rng1)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    s => {
      val (a, s1) = run(s)  
      (s1, f(a))
    } 
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    s => {
      val (a, s1) = run(s)  
      f(a) run (s1)
    }
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}
