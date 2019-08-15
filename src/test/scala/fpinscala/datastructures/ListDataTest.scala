package fpinscala.datastructures

import org.scalatest.FunSuite

class ListDataTest extends FunSuite {
  test("LinkedList.tail") {
    assert(LinkedList.tail(LinkedList(1, 2, 3)).toString === "Cons(2,Cons(3,Nil))")
    assert(LinkedList.tail(LinkedList(1)).toString === "Nil")
    assert(LinkedList.tail(Nil).toString === "Nil")
  }

  test("LinkedList.setHead") {
    assert(LinkedList.setHead(5, LinkedList(1, 2, 3)).toString === "Cons(5,Cons(2,Cons(3,Nil)))")
    assert(LinkedList.setHead(6, LinkedList(1)).toString === "Cons(6,Nil)")
    assert(LinkedList.setHead(7, Nil).toString === "Nil")
  }

  test("LinkedList.drop") {
    assert(LinkedList.drop(LinkedList(1, 2, 3), 1).toString === "Cons(2,Cons(3,Nil))")
    assert(LinkedList.drop(LinkedList(1, 2, 3), 2).toString === "Cons(3,Nil)")
    assert(LinkedList.drop(LinkedList(1, 2, 3), 3).toString === "Nil")
    assert(LinkedList.drop(LinkedList(), 3).toString === "Nil")
  }

  test("LinkedList.dropWhile") {
    assert(LinkedList.dropWhile(LinkedList(1, 2, 3), (x: Int) => x == -1).toString === "Cons(1,Cons(2,Cons(3,Nil)))")
    assert(LinkedList.dropWhile(LinkedList(1, 2, 3), (x: Int) => x == 1).toString === "Cons(2,Cons(3,Nil))")
    assert(LinkedList.dropWhile(LinkedList(1, 2, 3), (x: Int) => x < 3).toString === "Cons(3,Nil)")
    assert(LinkedList.dropWhile(LinkedList(1, 2, 3), (x: Int) => x < 11).toString === "Nil")
    assert(LinkedList.dropWhile(LinkedList(), (x: Int) => x != 1).toString === "Nil")
  }

  test("LinkedList.init") {
    assert(LinkedList.init(LinkedList(1, 2, 3)).toString === "Cons(1,Cons(2,Nil))")
    assert(LinkedList.init(LinkedList(1, 2)).toString === "Cons(1,Nil)")
    assert(LinkedList.init(LinkedList(1)).toString === "Nil")
    assert(LinkedList.init(LinkedList()).toString === "Nil")
  }

  test("LinkedList.length") {
    assert(LinkedList.length(LinkedList(1, 2, 3)) === 3)
    assert(LinkedList.length(LinkedList(1)) === 1)
    assert(LinkedList.length(LinkedList()) === 0)
  }

  test("LinkedList.sum3") {
    assert(LinkedList.sum3(LinkedList(1, 2, 3)) === 6)
    assert(LinkedList.sum3(LinkedList(1)) === 1)
    assert(LinkedList.sum3(LinkedList()) === 0)
  }

  test("LinkedList.product3") {
    assert(LinkedList.product3(LinkedList(1, 2, 3)) === 6.0)
    assert(LinkedList.product3(LinkedList(0, 2, 6)) === 0.0)
  }

  test("LinkedList.reverse") {
    assert(LinkedList.reverse(LinkedList(1, 2, 3)).toString === "Cons(3,Cons(2,Cons(1,Nil)))")
    assert(LinkedList.reverse(LinkedList(3, 2, 1)).toString === "Cons(1,Cons(2,Cons(3,Nil)))")
    assert(LinkedList.reverse(LinkedList(2, 1)).toString === "Cons(1,Cons(2,Nil))")
    assert(LinkedList.reverse(LinkedList(2)).toString === "Cons(2,Nil)")
    assert(LinkedList.reverse(LinkedList()).toString === "Nil")
  }

  test("LinkedList.reverse2") {
    assert(LinkedList.reverse2(LinkedList(1, 2, 3)).toString === "Cons(3,Cons(2,Cons(1,Nil)))")
    assert(LinkedList.reverse2(LinkedList(3, 2, 1)).toString === "Cons(1,Cons(2,Cons(3,Nil)))")
    assert(LinkedList.reverse2(LinkedList(2, 1)).toString === "Cons(1,Cons(2,Nil))")
    assert(LinkedList.reverse2(LinkedList(2)).toString === "Cons(2,Nil)")
    assert(LinkedList.reverse2(LinkedList()).toString === "Nil")
  }

  test("LinkedList.appendViaFoldRight") {
    assert(LinkedList.appendViaFoldRight(LinkedList(1, 2, 3), 4).toString === "Cons(1,Cons(2,Cons(3,Cons(4,Nil))))")
    assert(LinkedList.appendViaFoldRight(LinkedList(1), 2).toString === "Cons(1,Cons(2,Nil))")
    assert(LinkedList.appendViaFoldRight(LinkedList(), 1).toString === "Cons(1,Nil)")
  }

  test("LinkedList.concatList") {
    assert(LinkedList.concatList(LinkedList[List[Int]](LinkedList(1, 2), LinkedList(3, 4))).toString === "Cons(1,Cons(2,Cons(3,Cons(4,Nil))))")
    assert(LinkedList.concatList(LinkedList[List[Int]](LinkedList(4, 3), LinkedList(2, 1))).toString === "Cons(4,Cons(3,Cons(2,Cons(1,Nil))))")
    assert(LinkedList.concatList(LinkedList[List[Int]](LinkedList(3), LinkedList(2, 1))).toString === "Cons(3,Cons(2,Cons(1,Nil)))")
    assert(LinkedList.concatList(LinkedList[List[Int]](LinkedList(), LinkedList(2, 1))).toString === "Cons(2,Cons(1,Nil))")
    assert(LinkedList.concatList(LinkedList[List[Int]](LinkedList(2, 1), LinkedList())).toString === "Cons(2,Cons(1,Nil))")

  }

  test("LinkedList.addOne") {
    assert(LinkedList.addOne(LinkedList(1, 2, 3)).toString === "Cons(2,Cons(3,Cons(4,Nil)))")
    assert(LinkedList.addOne(LinkedList(1)).toString === "Cons(2,Nil)")
    assert(LinkedList.addOne(LinkedList()).toString === "Nil")
  }

}
