sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(n) => n
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l, f), map(r, f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))
    case Leaf(a) => f(a)
  }

  def sizeF[A, B](t: Tree[A]): Int = {
    fold(t, (_: A) => 1, (a: Int, b: Int) => 1 + a + b)
  }

  def depthF[A](t: Tree[A]): Int = {
    fold(t, (_: A) => 0, (a: Int, b: Int) => 1 + (a max b))
  }

  def maximumF(t: Tree[Int]): Int = {
    fold(t, (a: Int) => a, (a: Int, b: Int) => a max b)
  }

  def mapF[A, B](t: Tree[A], f: A => B): Tree[B] = {
    fold(t, (a: A) => Leaf(f(a)): Tree[B], (a:Tree[B], b:Tree[B]) => Branch(a, b))
  }

}

object practiceT {
  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(2), Leaf(8)), Branch(Leaf(2), Leaf(3)))
    println(Tree.mapF(t, (a:Int) => a*2))
  }
}
