import StreamT._

sealed trait StreamT[+A] {
  def headOption_01: OptionT[A] = this match {
    case Empty => None
    case Const(h, t) => Some(h())
  }

  //no tail recursive
  def toList_01: ListT[A] = this match {
    case Empty => Nil
    case Const(h, t) => Cons(h(), t().toList_01)
  }


  def take(n: Int): StreamT[A] = this match {
    case Const(h, t) if n > 1 => const(h(), t().take(n - 1))
    case Const(h, _) if n == 1 => const(h(), empty)
    case _ => empty
  }

  def drop(n: Int): StreamT[A] = this match {
    case Const(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile_01(p: A => Boolean): StreamT[A] = {
    @annotation.tailrec
    def go(s: StreamT[A], acc: StreamT[A]): StreamT[A] = s match {
      case Const(h, t) => if (p(h())) go(t(), const(h(), acc)) else go(t(), acc)
      case _ => acc
    }

    go(this, StreamT())
  }

  def exists_01(p: A => Boolean): Boolean = this match {
    case Const(h, t) => p(h()) || t().exists_01(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Const(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): StreamT[A] = foldRight(empty[A])((a, b) => {
    if (p(a))
      const(a, b)
    else
      b
  })

  def headOption: OptionT[A] = foldRight(None: OptionT[A])((a, _) => Some(a))

  def map[B](f: A => B): StreamT[B] = foldRight(empty[B])((a, b) => const(f(a), b))

  def filter(f: A => Boolean): StreamT[A] = foldRight(empty[A])((a, b) => if (f(a)) b else const(a, b))

  def append[B >: A](l: => StreamT[B]): StreamT[B] = foldRight(l)((a, b) => const(a, b))

  def flatMap[B](f: A => StreamT[B]): StreamT[B] = foldRight(empty[B])((h, t) => f(h) append t)

  def find(p: A => Boolean): OptionT[A] = filter(p).headOption

  def mapViaUnfold[B](f: A => B): StreamT[B] =
    unfold(this) {
      case Const(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): StreamT[A] =
    unfold((this, n)) {
      case (Const(h, t), 1) => Some((h(), (empty, 0)))
      case (Const(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): StreamT[A] =
    unfold(this) {
      case Const(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: StreamT[B])(f: (A, B) => C): StreamT[C] =
    unfold((this, s2)) {
      case (Const(h1, t1), Const(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: StreamT[B]): StreamT[(A,B)] = zipWith(s2)((_,_))

  def zipAll[B](s2: StreamT[B]): StreamT[(Option[A],Option[B])] = zipAll

  def toList: ListT[A] = {
    @annotation.tailrec
    def go(s: StreamT[A], acc: ListT[A]): ListT[A] = s match {
      case Const(h, t) => go(t(), Cons(h(), acc))
      case _ => acc
    }

    go(this, ListT())
  }
}

case object Empty extends StreamT[Nothing]

case class Const[+A](h: () => A, t: () => StreamT[A]) extends StreamT[A]

object StreamT {
  def const[A](hd: => A, tl: => StreamT[A]): StreamT[A] = {
    lazy val head = hd
    lazy val tail = tl
    Const(() => head, () => tail)
  }

  def empty[A]: StreamT[A] = Empty

  //def ones: StreamT[Int] = const(1, ones) -> Infinite stream
  //Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
  def constant_01[A](a: A): StreamT[A] = const(a, constant_01(a))

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.
  def constant[A](a: A): StreamT[A] = {
    lazy val tail: StreamT[A] = Const(() => a, () => tail)
    tail
  }

  def from_01(n: Int): StreamT[Int] = const(n, from_01(n + 1))

  val fibs_01 = {
    def go(f0: Int, f1: Int): StreamT[Int] = const(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => OptionT[(A, S)]): StreamT[A] = f(z) match {
    case Some((h, s)) => const(h, unfold(s)(f))
    case None => empty
  }

  //fibonacci with unfold
  def fibs(n: Int): StreamT[Int] = {
    unfold((0, 1))((x: (Int, Int)) => if (x._2 < n) Some(x._1, (x._2, x._1 + x._2)) else None)
  }

  /*OR ACCORDING TO THE BOOK
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.
  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... },
   but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  //from with unfold
  def from(n: Int): StreamT[Int] = unfold(n)((x: Int) => Some(x, x + 1))

  //constant with unfold
  def constantViaUnfold[A](a: A): StreamT[A] = unfold(a)((x: A) => Some(x, x)) //or unfold(a)(_ => Some((a,a)))


  def apply[A](as: A*): StreamT[A] =
    if (as.isEmpty) empty else const(as.head, apply(as.tail: _*))
}

object practiceStream {
  def main(args: Array[String]): Unit = {
    val xs = StreamT(0, 1, 2, 3, 4, 5, 6, 7)
    println(s"---->> ${xs.takeWhileViaUnfold(a => a % 2 == 0).toList}")

    /**
      * Infinite streams and corecursion
      * Because they’re incremental, the functions we’ve written also work for infinite streams.
      * Here’s an example of an infinite Stream of 1s:
      *
      * @return
      */
    //def ones: StreamT[Int] = const(1, ones)

    //println(s"${ones.take(5).toList}")
    //println(s"${ones.map(_ + 1).exists(_ % 2 == 0)}")
    //println(s"${ones.takeWhile(_ == 1).toList}")
    //println(s"${ones.forAll(_ == 1)}") //StackOverflowError

  }
}
