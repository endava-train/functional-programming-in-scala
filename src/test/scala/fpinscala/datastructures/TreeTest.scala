package fpinscala.datastructures

import org.scalatest.FunSuite

class TreeTest extends FunSuite {

  test("Tree.size") {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val nodes = Tree.size(tree)
    assert(nodes === 7)
  }

  test("Tree.max") {
    val tree = Branch(Branch(Leaf(5), Leaf(7)), Branch(Leaf(-10), Leaf(10)))
    val nodes = Tree.max(tree)
    assert(nodes === 10)
  }

  test("Tree.depth") {
    val tree = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val nodes = Tree.depth(tree)
    assert(nodes === 3)
  }

  test("Tree.map") {
    val tree = Branch(Branch(Leaf(5), Leaf(7)), Branch(Leaf(-10), Leaf(10)))
    val stringTree = Tree.map(tree)(x => x + 1).toString
    assert(stringTree === "Branch(Branch(Leaf(6),Leaf(8)),Branch(Leaf(-9),Leaf(11)))")
  }

  test("Tree.fold") {
    val tree = Branch(Branch(Leaf(5), Leaf(7)), Branch(Leaf(-10), Leaf(10)))
    val nodes = Tree.fold(tree)(a => 1)((a, b) => a + b + 1)
    assert(nodes === 7)
    val maxNode = Tree.fold(tree)(a => a)((a, b) => a max b)
    assert(maxNode === 10)
    val depthTree = Tree.fold(tree)(a => 1)((a, b) => (a max b) + 1)
    assert(depthTree === 3)
  }


}

