
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] = {
    map(f) getOrlElse None
  }

  def getOrlElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def isEmpty: Boolean = this match {
    case Some(_) => false
    case _ => true
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


case class Employee(name: String, department: String)

object Option {
  def lookupByName(name: String): Option[Employee] = Some(Employee("fabian", "2 departments"))

  val joeDepartment: Option[String] = lookupByName("Joe").map(_.department)
  val joeManager: Option[String] = lookupByName("Joe").flatMap(item => Some(item.department))

  def empty[A]: Option[A] = None
  //val a: Option[Int] => Option[Int] = lift(math.abs)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f


  //use for-comprehension
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  //for-comprehension (similar to flatMap and map together)
  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 10d

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {
      age.toInt
    }
    val optTickets: Option[Int] = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    List.foldLeft(a, Some(Nil): Option[List[A]])((a, b) => b flatMap (aa => a map (bb => Cons(aa, bb))))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    List.foldRight(a, Some(Nil): Option[List[A]])((x, y) => map2(x, y)(Cons(_, _)))

  def sequence_2[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h flatMap (hh => sequence_2(t) map (Cons(hh, _)))
    }

  //this is inefficient, since it traverses the list twice, first to convert each
  //String to an Option[Int] , and a second pass to combine these Option[Int] values
  //into an Option[List[Int]] the best way is def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]
  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(List.map(a)(i => Try(i.toInt)))

  //see before comment
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    List.foldLeft(a, Some(Nil): Option[List[B]])((a, b) => a flatMap (aa => f(b) map (bb => Cons(bb, aa))))
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldRight(a, Some(Nil): Option[List[B]])((h, t) => map2(f(h), t)(Cons(_, _)))

  def main(args: Array[String]): Unit = {

    val list = List(Some(10), Some(2), Some(2), Some(3), Some(5))
    val list2 = List("1", "2", "3")

    //    println(sequence(list)) // map from List(Option) to Option(List)
    //    println(sequence_1(list)) // map from List(Option) to Option(List)
    //    println(parseInts(list2)) // parse to int a list of string (inefficient)
    println(traverse(list2)(i => Try(i.toInt))) // parse to int a list of string (efficient)
  }
}