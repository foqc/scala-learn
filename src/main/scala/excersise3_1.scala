
object MyWorld {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n - 1 < 0) true
      else if (ordered(as(n), as(n - 1))) false
      else loop(n - 1)
    }

    loop(as.length - 1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    print(isSorted(Array(5, 4, 3, 2, 1), (n1: Int, n2: Int) => n1 > n2))
  }
}
