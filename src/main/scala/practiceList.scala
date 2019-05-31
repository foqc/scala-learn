sealed trait ListT[+A] {
  //no tail recursive
  def foldRight[A, B](l: ListT[A], b: B, f: (A, B) => B): B = {
    l match {
      case Nil => b
      case Cons(h, t) => f(h, foldRight(t, b, f))
    }
  }
}

case object Nil extends ListT[Nothing]

case class Cons[+A](head: A, tail: ListT[A]) extends ListT[A]

object ListT {
  def sum(ints: ListT[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: ListT[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def length[A](l: ListT[A]): Int = {

    @annotation.tailrec
    def loop(l: ListT[A], n: Int): Int = {
      l match {
        case Cons(_, t) => loop(t, n + 1)
        case Nil => n
      }
    }

    loop(l, 0)
  }

  def getTail[A](l: ListT[A]): ListT[A] = {
    l match {
      case Cons(_, t) => t
      case Nil => Nil
    }
  }

  def drop[A](l: ListT[A], n: Int): ListT[A] = {
    if (n <= 0) l
    else drop(getTail(l), n - 1)
  }

  def dropWhile[A](l: ListT[A], f: A => Boolean): ListT[A] = {
    l match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, dropWhile(t, f))
      case Nil => l
    }
  }

  def append[A](l: ListT[A], l2: ListT[A]): ListT[A] = {
    l match {
      case Cons(h, t) => Cons(h, append(t, l2))
      case Nil => l2
    }
  }

  def init[A](l: ListT[A]): ListT[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def sumR(l: ListT[Int]) = l.foldRight(l, 0, (a: Int, b: Int) => a + b);

  def productR(l: ListT[Double]) = l.foldRight(l, 1.0, (a: Double, b: Double) => a * b);

  def lengthR[A](l: ListT[A]) = l.foldRight(l, 0, (_: A, b: Int) => b + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: ListT[A], b: B, f: (B, A) => B): B = {
    l match {
      case Nil => b
      case Cons(h, t) => {
        println(s"foldLeft(${t},f(${b}, ${h}))")
        foldLeft(t, f(b, h), f)
      }
    }
  }

  def sumL(l: ListT[Int]) = foldLeft(l, 0, (a: Int, b: Int) => a + b);

  def productL(l: ListT[Double]) = foldLeft(l, 1.0, (a: Double, b: Double) => a * b);

  def lengthL[A](l: ListT[A]) = foldLeft(l, 0, (a: Int, _: A) => a + 1)

  def reverse[A](l: ListT[A]): ListT[A] = foldLeft(l, Nil, (a: ListT[A], b: A) => Cons(b, a))

  def foldRightViaFoldLeft[A, B](l: ListT[A], z: B, f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b, (g: B => B, a: A) => (b: B) => g(f(a, b)))(z)
  }

  def appendViaFoldRight[A](l: ListT[A], l2: ListT[A]): ListT[A] = foldRightViaFoldLeft(l, l2, (h: A, t: ListT[A]) => Cons(h, t))

  def concat[A](l: ListT[ListT[A]]): ListT[A] =
    foldRightViaFoldLeft(l, Nil, (h: ListT[A], t: ListT[A]) => appendViaFoldRight(h, t))

  def map[A, B](l: ListT[A], f: A => B): ListT[B] = {
    foldRightViaFoldLeft(l, Nil, (a: A, b: ListT[B]) => Cons(f(a), b))
  }

  def filter[A](l: ListT[A], f: A => Boolean): ListT[A] = {
    foldRightViaFoldLeft(l, Nil, (a: A, b: ListT[A]) => if (f(a)) b else Cons(a, b))
  }

  def flatMap[A, B](l: ListT[A], f: A => ListT[B]): ListT[B] = concat(map(l, f))

  def filterViaFlatMap[A](l: ListT[A], f: A => Boolean): ListT[A] =
    flatMap(l, (a: A) => if (f(a)) ListT(a) else Nil)

  def zipWith[A, B, C](a: ListT[A], b: ListT[B], f: (A, B) => C): ListT[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
  }

  def take[A](l: ListT[A], n: Int): ListT[A] = {

    def loop(l: ListT[A], n: Int, l2: ListT[A]): ListT[A] = {
      if (n == 0) {
        l2
      } else {
        l match {
          case Nil => Nil
          case Cons(h, t) => loop(t, n - 1, Cons(h, l2))
        }
      }
    }

    loop(l, n, Nil)
  }

  @annotation.tailrec
  def startsWith[A](l: ListT[A], prefix: ListT[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: ListT[A], sub: ListT[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  def apply[A](as: A*): ListT[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object practiceList {
  def main(args: Array[String]): Unit = {
    val x = ListT(1, 2, 3, 4, 5, 6, 7, 8)
    val y = ListT(4, 5, 6)

    //println(ListT.zipWith(x, y, (a: Int, b: Int) => a + b))
    //println(ListT.hasSubsequence(x, y))

  }
}
