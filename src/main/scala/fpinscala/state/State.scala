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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, s) = rng.nextInt

    val randomInt =
      if (i < 0) i + 1 + Int.MaxValue
      else i

    (randomInt, s)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, s) = rng.nextInt
    val d: Double = (1.0 + i + Int.MaxValue) / (2.0 * Int.MaxValue + 2)
    (d, s)
  }

  def doubleViaRand: Rand[Double] =
    map(int)(i => (1.0 + i + Int.MaxValue) / (2.0 * Int.MaxValue + 2))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s0) = rng.nextInt
    val (d, s1) = double(s0)
    ((i, d), s1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), s) = intDouble(rng)
    ((d, i), s)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    (1 to count).foldLeft[(List[Int], RNG)]((List.empty[Int], rng)) {
      case ((list, r), _) => {
        val (nextI, nextR) = r.nextInt
        (nextI :: list, nextR)
      }
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(List.empty[A]))((rngA, rngList) =>
      map2(rngA, rngList)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    {
      val (a, s1) = run(s)
      (f(a), s1)
    }
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, sa) = run(s)
    val (b, sb) = run(sa)
    (f(a, b), sb)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, sa) = run(s)
    f(a).run(sa)
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A) = State[S, A](s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight[State[S, List[A]]](
    unit[S, List[A]](List())
  ){
    (s, list) => s.map2(list)(_ :: _)
  }


  def oneStep(input: Input): State[Machine, (Int, Int)] = State[Machine, (Int, Int)]{ machine =>
    input match {
      case Coin if machine.locked && machine.candies > 0 =>
        val newMachine = machine.copy(locked = false, coins = machine.coins + 1)
        ((machine.coins, machine.candies), newMachine)
      case Coin if machine.locked =>
        val newMachine = machine.copy(locked = false, coins = machine.coins + 1)
        ((machine.coins, machine.candies), newMachine)



//TODO ...
      case Coin if !machine.locked => ((machine.coins, machine.candies), machine)
      case Turn if machine.locked => ((machine.coins, machine.candies), machine)
      case Turn if !machine.locked => ((machine.coins, machine.candies), machine)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
