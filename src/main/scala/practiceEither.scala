sealed trait EitherT[+E, +A] {
  def map[B](f: A => B): EitherT[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => EitherT[EE, B]): EitherT[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => EitherT[EE, B]): EitherT[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: EitherT[EE, B])(f: (A, B) => C): EitherT[EE, C] = {
    this flatMap (aa => b map (bb => f(aa, bb)))
  }

  //or
  def map2_01[EE >: E, B, C](b: EitherT[EE, B])(f: (A, B) => C): EitherT[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}

case class Left[+E](value: E) extends EitherT[E, Nothing]

case class Right[+A](value: A) extends EitherT[Nothing, A]

object EitherT {
  def mean(xs: IndexedSeq[Double]): EitherT[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): EitherT[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }


  def traverse[E, A, B](l: ListT[A])(f: A => EitherT[E, B]): EitherT[E, ListT[B]] = {
    l foldRight(l, Right(Nil), (x: A, y: EitherT[E, ListT[B]]) => f(x).map2(y)((h: B, t: ListT[B]) => Cons(h, t)))
  }

  def sequence[E, A](l: ListT[EitherT[E, A]]): EitherT[E, ListT[A]] = {
    traverse(l)((x: EitherT[E, A]) => x)
  }
}

object Employee2 {

  def Try[A](a: => A): EitherT[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets


  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): EitherT[Exception, Double] =
    for {
      a <- Try {
        age.toInt
      }
      tickets <- Try {
        numberOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, tickets)
}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {
  def mkName(name: String): EitherT[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): EitherT[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): EitherT[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  //without lazy (runs twice (i + i))
  def maybeTwice2(b: Boolean, i: => Int) = {
    if (b) i+i else 0
  }

  //with lazy (runs once (y is lazy))
  def maybeTwice2_01(b: Boolean, i: => Int) = {
    lazy val y = i;
    if (b) y+y else 0
  }

  }

object practiceEither {
  def main(args: Array[String]): Unit = {
    val xs = ListT(Right(5), Right(6), Right(15))
    println(Person.maybeTwice2_01(true, { println("hi"); 1+41 }))
  }
}
