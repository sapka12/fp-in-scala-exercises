package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {

  "double" should "be between 0 and 1" in {

    val randoms = (1 to 1000000)
      .map(i => RNG.double(Simple(i.toLong))._1)

    assert(randoms.forall(d => 0 <= d && d < 1))
  }

  "double1" should "b1e between 0 and 1" in {
    val as = RNG.ints(3)(Simple(1L))
    println(as)
  }

  }
