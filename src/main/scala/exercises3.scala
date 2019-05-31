//sealed trait List[+A]
//
//case object Nil extends List[Nothing]
//
//case class Cons[+A](head: A, tail: List[A]) extends List[A]
//
//object List {
//  def sum(ints: List[Int]): Int = ints match {
//    case Nil => 0
//    case Cons(x, xs) => x + sum(xs)
//  }
//
//  def product(ds: List[Double]): Double = ds match {
//    case Nil => 1.0
//    case Cons(0.0, _) => 0.0
//    case Cons(x, xs) => x * product(xs)
//  }
//
//  /**
//    * GENERALIZING SUM AND PRODUCT
//    *
//    * @param as
//    * @param z
//    * @param f
//    * @tparam A
//    * @tparam B
//    * @return
//    */
//  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
//    as match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }
//
//  def sum2(ns: List[Int]) =
//    foldRight(ns, 0)((x, y) => x + y)
//
//  def product2(ns: List[Double]) =
//    foldRight(ns, 1.0)(_ * _)
//
//  def length[A](as: List[A]): Int =
//    foldRight(as, 0)((_, y) => y + 1)
//
//  /*
// It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation.
// If the function is not tail-recursive, it will yield a compile error, rather than silently compiling the code
// and resulting in greater stack space usage at runtime.
// */
//  @annotation.tailrec
//  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
//    case Nil => z
//    case Cons(h, t) => foldLeft(t, f(z, h))(f)
//  }
//
//  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
//
//  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
//
//  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)
//
//  /*
//  Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently returning a value just means this bug will be discovered later, further from the place where it was introduced.
//  It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the right hand side of a pattern. This makes it clear the value isn't relevant.
//  */
//  def tail[A](ds: List[A]): List[A] = ds match {
//    case Nil => sys.error("tail of empty list")
//    case Cons(_, xs) => xs
//  }
//
//  def setHead[A](h: A, ds: List[A]): List[A] = ds match {
//    case Nil => sys.error("setHead of empty list")
//    case Cons(_, xs) => Cons(h, xs)
//  }
//
//  /**
//    * 1 solution using tail function that is already created
//    *
//    * @param l
//    * @param n
//    * @tparam A
//    * @return
//    */
//  def drop[A](l: List[A], n: Int): List[A] = {
//    if (n < 0) l
//    else {
//      drop(tail(l), n - 1)
//    }
//  }
//
//  /**
//    * 2nd solution
//    *
//    * @param l
//    * @param n
//    * @tparam A
//    * @return
//    */
//  def drop9[A](l: List[A], n: Int): List[A] =
//    if (n <= 0) l
//    else l match {
//      case Nil => Nil
//      case Cons(_, t) => drop9(t, n - 1)
//    }
//
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
//    l match {
//      case Cons(h, t) if f(h) => dropWhile(t, f)
//      case _ => l
//    }
//
//  /**
//    * Implement a function, init, that returns a List
//    * consisting of all but the last element of a List
//    *
//    * @param l
//    * @tparam A
//    * @return
//    */
//  /*
//  Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive solution
//   will use a stack frame for each element of the list, which can lead to stack overflows for large lists (can you see why?).
//   With lists, it's common to use a temporary, mutable buffer internal to the function (with lazy lists or streams, which we
//   discuss in chapter 5, we don't normally do this). So long as the buffer is allocated internal to the function,
//   the mutation is not observable and RT is preserved.
//  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which doesn't
//  require even local mutation. We'll write a reverse function later in this chapter.
//  */
//  def init[A](l: List[A]): List[A] =
//  l match {
//    case Nil => sys.error("init of empty list")
//    case Cons(_, Nil) => Nil
//    case Cons(h, t) => Cons(h, init(t))
//  }
//
//  def init2[A](l: List[A]): List[A] = {
//    import collection.mutable.ListBuffer
//    val buf = new ListBuffer[A]
//
//    @annotation.tailrec
//    def go(cur: List[A]): List[A] = cur match {
//      case Nil => sys.error("init of empty list")
//      case Cons(_, Nil) => List(buf.toList: _*)
//      case Cons(h, t) => buf += h; go(t)
//    }
//
//    go(l)
//  }
//
//  def append[A](a1: List[A], a2: List[A]): List[A] =
//    a1 match {
//      case Nil => a2
//      case Cons(h, t) => Cons(h, append(t, a2))
//    }
//
//  /**
//    * e. By calling this function
//    * apply and placing it in the companion object, we can invoke it with syntax like
//    * List(1,2,3,4) or List("hi","bye"), with as many values as we want separated
//    * by commas (we sometimes call this the list literal or just literal syntax).
//    *
//    * @param as
//    * @tparam A
//    * @return
//    */
//  def apply[A](as: A*): List[A] =
//    if (as.isEmpty) Nil
//    else Cons(as.head, apply(as.tail: _*))
//}
//
///*object MyWorld {
//  def main(args: Array[String]): Unit = {
//    val x = List(1, 2, 3, 4, 5)
//    //drop(x)
//  }
//}*/
//
