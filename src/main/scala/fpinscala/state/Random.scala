package fpinscala.state

object Random {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

    def nonNegativeInt: (Int, RNG) = {
      val (i, s) = nextInt

      val randomInt =
        if (i < 0) i + 1 + Int.MaxValue
        else i

      (randomInt, s)
    }

//    def double: (Double, RNG) = {
//      val (i, s) = nonNegativeInt
//
//    }

  }

  val rng = SimpleRNG(1234L)

  rng.nonNegativeInt



}
