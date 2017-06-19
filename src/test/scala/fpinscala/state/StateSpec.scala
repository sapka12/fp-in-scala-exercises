package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {

  behavior of "double"

  it should "be between 0 and 1" in {

    val randoms = (1 to 1000000)
      .map(i => RNG.double(Simple(i.toLong))._1)

    assert(randoms.forall(d => 0 <= d && d < 1))
  }

  behavior of "ints"

  it should "show example" in {
    val as = RNG.ints(3)(Simple(1L))
    println(as)
  }

  behavior of "doubleViaRand"

  it should "be between 0 and 1" in {

    val randoms = (1 to 1000000)
      .map(i => RNG.doubleViaRand(Simple(i.toLong))._1)

    assert(randoms.forall(d => 0 <= d && d < 1))
  }

  behavior of "seq"

  it should "seq" in {

    val rand = Simple(1L)

    val d = RNG.doubleViaRand
    val s = RNG.map(RNG.int)(_.toString)

    val expectedD = d(rand)
    val expectedString = s(expectedD._2)

    val seq = RNG.sequence(List(d, s))

    val result = seq(Simple(1L))

    assert(expectedD._1 == result._1(0))
    assert(expectedString._1 == result._1(1))
  }

}
