
sealed trait OptionT[+A] {

  def map[B](f: A => B): OptionT[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => OptionT[B]): OptionT[B] = map(f) getOrElse None

  //or
  def flatMap_1[B](f: A => OptionT[B]): OptionT[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => OptionT[B]): OptionT[B] = this match {
    case None => ob
    case _ => this
  }

  //or
  def orElse_1[B >: A](ob: => OptionT[B]): OptionT[B] =
    this map (Some(_)) getOrElse ob


  def filter_1(f: A => Boolean): OptionT[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  //or
  def filter(f: A => Boolean): OptionT[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def lift[A, B](f: A => B): OptionT[A] => OptionT[B] = _ map f

  def abs0(a: OptionT[Double]): OptionT[Double] = {
    val f: OptionT[Double] => OptionT[Double] = lift(math.abs)
    f(a)
  }
}

case class Some[+A](value: A) extends OptionT[A]

case object None extends OptionT[Nothing]

object OptionT {

  def map2[A, B, C](a: OptionT[A], b: OptionT[B])(f: (A, B) => C): OptionT[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  //or using for comprehension
  def map2_02[A, B, C](a: OptionT[A], b: OptionT[B])(f: (A, B) => C): OptionT[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence_0[A](a: ListT[OptionT[A]]): OptionT[ListT[A]] =
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => h flatMap (hh => sequence_0(t) map ((t: ListT[A]) => Cons(hh, t)))
    }

  def sequence[A](a: ListT[OptionT[A]]): OptionT[ListT[A]] = {
    a foldRight(a, Some(Nil), (x: OptionT[A], y: OptionT[ListT[A]]) => map2(x, y)((h: A, t: ListT[A]) => Cons(h, t)))
  }

  def traverse[A, B](a: ListT[A])(f: A => OptionT[B]): OptionT[ListT[B]] = {
    a foldRight(a, Some(Nil), (x: A, y: OptionT[ListT[B]]) => map2(f(x), y)((h: B, t: ListT[B]) => Cons(h, t)))
  }

  //or
  def traverse_1[A, B](a: ListT[A])(f: A => OptionT[B]): OptionT[ListT[B]] =
    a match {
      case Nil => Some(Nil)
      case Cons(h, t) => map2(f(h), traverse_1(t)(f))(Cons(_, _))
    }

  //more efficient way of sequence
  def sequenceViaTraverse[A](a: ListT[OptionT[A]]): OptionT[ListT[A]] =
    traverse(a)((x: OptionT[A]) => x)
}

case class Employee(name: String, department: String)

object Employee {
  def lookupByName(name: String): OptionT[Employee] = Some(Employee("foqc", "it"))

  //val joeDepartment: OptionT[String] = lookupByName("foqc").map(a => a.department)

  //val joeManager: OptionT[String] = lookupByName("Joe").flatMap(a => Some(a.department))

  //val dept: String = lookupByName("Joe").map(_.department).filter(_ == "Accounting").getOrElse("Default Dept")

  def mean(xs: Seq[Double]): OptionT[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): OptionT[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets


  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): OptionT[Double] = {
    val optAge: OptionT[Int] = Try(age.toInt)
    val optTickets: OptionT[Int] = Try(numberOfSpeedingTickets.toInt)
    OptionT.map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): OptionT[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

}

object practiceO {

  def main(args: Array[String]): Unit = {
    val xs = ListT(Some(1.0), Some(2), Some(8))
    println(Some(2) filter((a: Int) => a > 1))
  }
}

