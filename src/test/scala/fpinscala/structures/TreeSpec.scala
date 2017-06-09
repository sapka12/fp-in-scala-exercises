package fpinscala.structures

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers{

  val aTree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  behavior of "Tree"

  it should "return size" in {
    Tree.size(aTree) shouldBe 5
  }

  it should "return max" in {
    Tree.maximum(aTree) shouldBe 3
  }

  it should "return depth" in {
    Tree.depth(Leaf("")) shouldBe 0
    Tree.depth(Branch(Leaf(1), Leaf(""))) shouldBe 1
    Tree.depth(aTree) shouldBe 2
  }

  it should "map" in {
    val x3: Int => Int = _ * 3
    Tree.map(aTree)(x3) shouldBe Branch(Leaf(3), Branch(Leaf(6), Leaf(9)))
  }

  behavior of "Tree via fold"

  it should "return size" in {
    Tree.sizeViaFold(aTree) shouldBe 5
  }

  it should "return max" in {
    Tree.maximumViaFold(aTree) shouldBe 3
  }

  it should "return depth" in {
    Tree.depthViaFold(Leaf("")) shouldBe 0
    Tree.depthViaFold(Branch(Leaf(1), Leaf(""))) shouldBe 1
    Tree.depthViaFold(aTree) shouldBe 2
  }

  it should "map" in {
    val x3: Int => Int = _ * 3
    Tree.mapViaFold(aTree)(x3) shouldBe Branch(Leaf(3), Branch(Leaf(6), Leaf(9)))
  }


}
