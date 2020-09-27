sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 10d

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try {
        age.toInt
      }
      tickets <- Try {
        numberOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, tickets)

  def parseInsuranceRateQuote2(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
    val a = Try {
      age.toInt
    }
    val b = Try {
      age.toInt
    }
    a.map2(b)(insuranceRateQuote)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    List.foldLeft(es, Right(Nil): Either[E, List[A]])((a, b) => b flatMap (bb => a map (aa => Cons(bb, aa))))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldLeft(as, Right(Nil): Either[E, List[B]])((a, b) => a flatMap (aa => f(b) map (bb => Cons(bb, aa))))

  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    List.foldRight(es, Right(Nil): Either[E, List[B]])((a, b) => f(a).map2(b)(Cons(_, _)))

  def main(args: Array[String]): Unit = {
    val list2 = IndexedSeq("1", "2", "3")
    //    println(safeDiv(0, 0))
    //    println(parseInsuranceRateQuote2("2", "4"))
    println(Person.mkPerson("", -3)) //validate person data
  }
}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

}