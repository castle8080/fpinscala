package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(tree: Tree[Int]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }
  
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  
  // My first attempt was enough to cover the case of depth :(
  // I took a look at the answers (skimmed it) - yeah I cheated.
  // fold looks kind of like map and reduce.
  //
  // def fold[A,B](tree: Tree[A], initial: B)(f: (A, B) => B): B = tree match {
  //  case Leaf(v) => f(v, initial)
  //  case Branch(l, r) => {
  //    val leftVal = fold(l, initial)(f)
  //    fold(r, leftVal)(f)
  //  }
  // }
  
  def fold[A,B](tree: Tree[A])(mapF: A => B)(reduceF: (B,B) => B): B = tree match {
    case Leaf(v) => mapF(v)
    case Branch(l, r) => reduceF(fold(l)(mapF)(reduceF), fold(l)(mapF)(reduceF))
  }
  
  def size2(tree: Tree[_]): Int =
    fold(tree)(_ => 1)(_ + _)

  def maximum2(tree: Tree[Int]): Int =
    fold(tree)(identity)(Math.max)

  def depth2(tree: Tree[Int]): Int =
    fold(tree)(_ => 1)(1 + _.max(_))
  
  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(i => Leaf(f(i)): Tree[B])(Branch.apply)
  
}