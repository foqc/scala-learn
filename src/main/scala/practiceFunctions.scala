object practiceFunctions {
  def main(args: Array[String]): Unit = {
    //println(fibonacci(5))
    print(isSorted(Array(5, 4, 3, 2, 1), (n1: Int, n2: Int) => n1 < n2))
  }

  def fibonacci(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  // def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
  def isSorted[A](ss: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= ss.length - 1) true
      else if (ordered(ss(n), ss(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  //def partial1[A,B,C](a: A, f: (A,B) => C): B => C
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // def curry[A,B,C](f: (A, B) => C): A => (B => C)
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  //def uncurry[A,B,C](f: A => B => C): (A, B) => C
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //def compose[A,B,C](f: B => C, g: A => B): A => C
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))
}
