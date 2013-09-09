object ch2 {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = 
    s"The absolute value of ${x} is ${abs(x)}"

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // exercise 1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, y: Int, n: Int): Int =
      if (n <= 0) x
      else go(y, x + y, n - 1)

    go(0, 1, n)
  }

  def findFirst[A](ds: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ds.length) -1
      else if (p(ds(n))) n
      else loop(n + 1)

    loop(0)
  }

  // exercise 2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i + 1 > as.length - 1)
        true
      else {
        val car = as(i)
        val cadr = as(i + 1)
        if (gt(car, cadr)) {
          go(i + 1)
        } else {
          false
        }
      }
    }

    go(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  // exercise 3
  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a,b))

  // exercise 4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    //(a: A) => f(g(a))
    f compose g
}
