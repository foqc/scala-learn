sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val leafL = Leaf(200)
    val leafR = Leaf(4)

    val leafL1 = Leaf(20)
    val leafR1 = Leaf(40)

    val branch: Branch[Int] = Branch(leafL1, leafR1)

    val branch2: Branch[Int] = Branch(leafL, leafR)

    val branchRoot: Branch[Int] = Branch(branch, branch2)

    //    println(size(branchRoot))// return size of tree (branches and leaves)
    //    println(maximum(branchRoot)) //returns the maximum element in a Tree[Int]
    //    println(depth(branchRoot)) //returns the maximum path length from the root of a tree to any leaf.
    println(map(branchRoot)(x => x + 2)) //apply map to tree
  }
}