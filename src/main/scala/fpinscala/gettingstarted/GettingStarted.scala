package fpinscala.gettingstarted

object GettingStarted {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def format(name: String, x: Int, f: Int => Int): String = {
    val msg = "The %s value of %d is %d."
    msg.format(name, x, f(x))
  }

  def facto(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
        if (n < 2) acc
        else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fibo(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int = {
      if (n == 0) a
      else go(n - 1, b, a + b)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int): Boolean = {
      if (index == as.length) true
      else if (!ordered(as(index - 1), as(index))) false
      else go(index + 1)
    }

    go(Math.min(1, as.length))
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
