package fpinscala.gettingstarted

import org.scalatest.FunSuite

class GettingStartedTest extends FunSuite {
  test("GettingStarted.abs") {
    assert(GettingStarted.abs(-3) === 3)
    assert(GettingStarted.abs(3) === 3)
    assert(GettingStarted.abs(0) === 0)
  }

  test("GettingStarted.format") {
    assert(GettingStarted.format("absolute", -3, GettingStarted.abs) === "The absolute value of -3 is 3.")
    assert(GettingStarted.format("absolute", 0, GettingStarted.abs) === "The absolute value of 0 is 0.")
    assert(GettingStarted.format("absolute", 3, GettingStarted.abs) === "The absolute value of 3 is 3.")
    assert(GettingStarted.format("factorial", 5, GettingStarted.facto) === "The factorial value of 5 is 120.")
    assert(GettingStarted.format("fibonacci", 5, GettingStarted.fibo) === "The fibonacci value of 5 is 5.")
  }

  test("GettingStarted.facto") {
    assert(GettingStarted.facto(0) === 1)
    assert(GettingStarted.facto(1) === 1)
    assert(GettingStarted.facto(5) === 120)
  }

  test("GettingStarted.fibo") {
    val input: Array[Int] =       Array(0, 1, 2, 3, 4, 5)
    val fiboNumbers: Array[Int] = Array(0, 1, 1, 2, 3, 5)
    for ((x, fibo) <- input.zip(fiboNumbers)) {
      assert(GettingStarted.fibo(x) === fibo)
    }
  }

  test("GettingStarted.isSorted") {
    val numbersInt: Array[Int] = Array(1, 10, 20, 30, 40)
    val numberShort: Array[Short] = Array(0)
    val numberDouble: Array[Double] = Array(40.0, 20.0, 80)
    assert(GettingStarted.isSorted(numbersInt, (a: Int, b: Int) => a - b <= 0))
    assert(GettingStarted.isSorted(numberShort, (a: Short, b: Short) => a - b <= 0))
    assert(GettingStarted.isSorted(Array(), (a: Double, b: Double) => a - b <= 0))
    assert(!GettingStarted.isSorted(numberDouble, (a: Double, b: Double) => a - b <= 0))

  }

  test("GettingStarted.curry with sum") {
    val sum = (a: Int, b: Int) => a + b
    val sumCurry = GettingStarted.curry(sum)
    assert(sumCurry(2)(5) === sum(2, 5))
  }

  test("GettingStarted.curry") {
    val sum = (a: Int, b: Int) => a + b
    val sumCurry = GettingStarted.curry(sum)
    assert(sumCurry(2)(5) === sum(2, 5))

    val modNeg = (a: Int, m: Int) => ((a % m) + m) % m
    val modNegCurry = GettingStarted.curry(modNeg)
    assert(modNegCurry(-5)(4) === modNeg(-5, 4))
  }

  test("GettingStarted.uncurry") {
    def modNegCurry(a: Int)(m: Int): Int = ((a % m) + m) % m
    val modNeg = GettingStarted.uncurry(modNegCurry)
    assert(modNegCurry(-5)(4) === modNeg(-5, 4))
  }

  test("GettingStarted.compose") {
    val pow2 = (a: Int) => Math.pow(2, a).toInt
    val pow3 = (a: Int) => Math.pow(3, a).toInt
    val comppsed = GettingStarted.compose(pow2, pow3)

    assert(comppsed(2) === pow2(pow3(2)))
  }



}
