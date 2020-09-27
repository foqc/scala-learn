import Stream.cons
import Stream.empty
import Stream.unfold

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case ConsStream(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case ConsStream(h, t) => Cons(h(), t().toList)
  }

  def take(n: Int): Stream[A] = this match {
    case ConsStream(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case ConsStream(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case ConsStream(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case ConsStream(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def exists(p: A => Boolean): Boolean = this match {
    case ConsStream(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case ConsStream(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](l: => Stream[B]): Stream[B] = foldRight(l)((a, b) => cons(a, b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case ConsStream(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (ConsStream(h, _), 1) => Some((h(), (empty, 0)))
      case (ConsStream(h, t), _) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case ConsStream(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (ConsStream(h1, t1), ConsStream(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (ConsStream(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, ConsStream(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (ConsStream(h1, t1), ConsStream(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that `s2` is exhausted.
  If `s` is exhausted first, or we find an element that doesn't match, we terminate early. Using non-strictness,
  we can compose these three separate logical steps--the zipping, the termination when the second stream is exhausted,
  and the termination if a nonmatching element is found or the first stream is exhausted.
  */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(a => !a._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
  */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)


  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

case class ConsStream[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail: Stream[A] = tl
    ConsStream(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones) // infinite ones

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a)) // infinite value
  //more efficient
  def constant1[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = ConsStream(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1)) //infinite stream of integers, starting from n , then n+ 1 , n + 2 , and so on

  def fibs(n: Int, n1: Int): Stream[Int] = Stream.cons(n, fibs(n1, n + n1))

  //another way
  val fibs1 = {
    def go(f0: Int, f1: Int): Stream[Int] = cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
Scala provides shorter syntax when the first action of a function literal is to match on an expression.
The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`,
but we avoid having to choose a name for `p`, only to pattern match on it.
*/
  val fibsViaUnfold = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  val fibsViaUnfold1 = unfold((0, 1))(f => Some((f._1, (f._2, f._1 + f._2))))

  def fromViaUnfold(n: Int) = unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A) = unfold(a)(_ => Some((a, a)))

  // could also of course be implemented as constant(1)
  val onesViaUnfold = unfold(1)(_ => Some((1, 1)))

  def main(args: Array[String]): Unit = {
    val list = Stream(10, 2, 2, 3, 5)
    val list2 = Stream(40, 20, 90)
    //    println(list.toList) //stream to list
    //    println(list.headOption) //get head from stream
    //    println(list.headOptionViaFoldRight) //get head from stream via foldRight
    //    println(list.take(2).toList) //get n elements from stream and convert to list
    //    println(list.drop(2).toList) //delete n elements from stream and convert the rest to list
    //    println(list.takeWhile(a => a % 2 == 0).toList) //delete n elements from stream while condition is true and convert the rest to list
    //    println(list.existsViaFoldRight(a => a == 0)) //check if exist element in stream
    //    println(list.forAll(a => a % 2 == 0)) //check if all elements meet the condition in stream
    //    println(list.map(_ + 2).toList) //map stream
    //    println(list.filter(_ % 2 == 0).toList) //filter stream
    //    println(list.append(list2).toList) //append stream
    //    println(ones.take(5).toList) //infinite stream stream
    //    println(ones.takeWhile(_ == 1).toList) //infinite stream stream (stackoverflow)
    //    println(constant("a").take(2).toList) //infinite stream stream of given data
    //    println(from(1).take(2).toList) //infinite stream of integers, starting from n , then n+ 1 , n + 2 , and so on
    //    println(fibs(0, 1).take(6).toList) //fibonacci using infinite stream
    //    println(list.mapViaUnfold((a: Int) => a + 2).take(6).toList) //fibonacci using infinite stream and unfold
    println(list.tails.toList) //fibonacci using infinite stream and unfold
  }

}