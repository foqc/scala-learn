import java.text.SimpleDateFormat
import java.util.TimeZone

import scala.collection.immutable.Stream.Empty
import scala.util.Try

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ints: List[Int]): Int = ints match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  /*
  It's not a tail recursive
   */
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /*
It's a tail recursive
 */
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Cons(_, _), 0) => l
    case (Nil, _) => Nil
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    def loop(l: List[A], nl: List[A]): List[A] = l match {
      case Nil => nl
      case Cons(x, xs) => if (f(x)) loop(xs, nl) else loop(xs, Cons(x, nl))
    }

    loop(l, Nil)
  }

  def append[A](l: List[A], nl: List[A]): List[A] = l match {
    case Nil => nl
    case Cons(x, xs) => Cons(x, append(xs, nl))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x, y) => Cons(y, x))

  def appendViaFoldRight[A](l: List[A], nl: List[A]): List[A] = foldRight(l, nl)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())((x: List[A], y: List[A]) => append(x, y))

  def addOne[A](l: List[Int]): List[Int] = foldRight(l, List[Int]())((x, y) => Cons(x + 1, y))

  def doubleToString[A](l: List[Double]): List[String] = foldRight(l, List[String]())((x, y) => Cons(x.toString, y))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((x, y) => Cons(f(x), y))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def flatMap_2[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]())((x, y) => append(f(x), y))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) Nil else List(x))

  def oneList(l: List[Int], nl: List[Int]): List[Int] = (l, nl) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(x2, x2s)) => Cons(x + x2, oneList(xs, x2s))
  }

  def zipWith[A, B, C](l: List[A], nl: List[B])(f: (A, B) => C): List[C] = (l, nl) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(x2, x2s)) => Cons(f(x, x2), zipWith(xs, x2s)(f))
  }

  def take[A](l: List[A], n: Int): List[A] =
    if (n <= 0) Nil
    else l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x, take(xs, n - 1))
    }

  def takeWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, takeWhile(xs)(f)) else takeWhile(xs)(f)
  }

  def forAll[A](l: List[A])(f: A => Boolean): Boolean = l match {
    case Nil => true
    case Cons(x, xs) => if (f(x)) forAll(xs)(f) else false
  }

  def exist[A](l: List[A])(f: A => Boolean): Boolean = l match {
    case Nil => false
    case Cons(x, xs) => if (f(x)) true else exist(xs)(f)
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

  def main(args: Array[String]): Unit = {
    val list = List(10, 2, 2, 3, 5)
    val list2 = List(-10, -2, -0, -3, -5)
    val list3 = List(list, list2)
    //    println(sum(list))
    //    println(product(list))
    //    println(tail(list))
    //    println(setHead(list, 4.45))
    //    println(drop(list, 4))
    //    println(dropWhile(list, (x: Int) => x % 2 != 0))
    //    println(append(list, list2))
    //    println(init(list))
    //    println(foldRight(list, Nil: List[Int])((x, y) => Cons(x, y))) // get list
    //    println(length(list)) // get length of list using foldRight
    //    println(foldLeft(list, Nil: List[Int])((x, y) => Cons(y, x))) // get list
    //    println(reverse(list)) // get list
    //    println(appendViaFoldRight(list, list2)) // append list
    //    println(concat(list3)) // concat list
    //    println(addOne(list)) // add one to list elements
    //    println(doubleToString(list)) // covert double to string to list elements
    //    println(map(list)(x => x + 2)) // map each elements of list and return
    //    println(filter(list)(x => x % 2 == 0)) // filter each elements of list and return
    //    println(flatMap(list)(x => List(x, x))) // flat and Map each elements of list and return
    //    println(flatMap_2(list)(x => List(x, x))) // flat and Map elements of list and return
    //    println(filterViaFlatMap(list)(x => x % 2 != 0)) // filter each elements of list and return
    //    println(oneList(list, list2)) // sum each element of tow list
//        println(zipWith(list, list2)((x, y) => x.toString + " : " + y.toString)) // sum each element of two list (more generic)
    //    println(take(list, 1)) // take n elements of list
    //    println(takeWhile(list)(x => x != 2)) // take n elements of list when predicate is true
    //    println(forAll(list)(x => x >= 2)) // return true for all elements that meet the rule
//    println(exist(list)(x => x == 2)) // return true for any of elements that meet the rule
//    println(hasSubsequence(list, List(10, 2, 2))) // return true if list has a sublist on it
    val TIMESTAMP_UTC = "Etc/GMT"
    val date2 = "02-02-1955"
    val date3 = "31-12-1969"
    val date = "02-01-1970"
    val DEFAULT_DATE = "dd-MM-yyyy"
    val formatter = new SimpleDateFormat(DEFAULT_DATE)
    val utc0 = TimeZone.getTimeZone(TIMESTAMP_UTC)

    formatter.setTimeZone(utc0)
    val r = Try(formatter.parse(date))
    print(r, " ::::: ",  r.map(_.getTime))
  }

}