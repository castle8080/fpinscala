package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(h, t) => t
      case Nil => throw new UnsupportedOperationException("tail of an empty list.")
    }

  def setHead[A](l: List[A], h: A): List[A] =
    Cons(h, tail(l))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(_, rest) if (n > 0) => drop(rest, n-1)
      case _ => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, rest) if (f(h)) => dropWhile(rest, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new UnsupportedOperationException("init of an empty list.")
      case Cons(i, Nil) => Nil
      case Cons(i, rest) => Cons(i, init(rest))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0) { (_, n) => n + 1 }
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(item, rest) => foldLeft(rest, f(z, item))(f)
    }
  
  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)
  
  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  
  def length2(l: List[_]) = foldLeft(l, 0) { (n, _) => n + 1 }
  
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) { (rl, item) => Cons(item, rl) }
  
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = 
    foldLeft(reverse(l), z) { (b,a) => f(a,b) }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z) { (item, acc) => f(acc, item) }
  
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons.apply)
    
  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2) { (list, item) => Cons(item, list) }
  
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil: List[A])(append2)
  
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int]) { (item, acc) => Cons(item + 1, acc) }
    
  def doubleToStr(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String]) { (item, acc) => Cons(item.toString, acc) }   
    
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B]) { (item, acc) => Cons(f(item), acc) }
  
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) { (item, acc) => if (f(item)) Cons(item, acc) else acc }
 
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B]) { (item, acc) => append2(f(item), acc) }
  
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { item => if (f(item)) List(item) else Nil }
   
  def combineIntLists(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Cons(i1, lr1), Cons(i2, lr2)) => Cons(i1 + i2, combineIntLists(lr1, lr2))
      case _ => Nil
    }
  
  
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Cons(i1, lr1), Cons(i2, lr2)) => Cons(f(i1, i2), zipWith(lr1, lr2)(f))
      case _ => Nil
    }
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    
    def startsWith(sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Cons(i1, r1), Cons(i2, r2)) => i1 == i2 && startsWith(r1, r2)
      case _ => false
    }
    
    def go(sup: List[A], sub: List[A]): Boolean = sup match {
      case _ if (startsWith(sup, sub)) => true
      case Cons(_, supR) => go(supR, sub)
      case _ => false
    }
 
    go(sup, sub)
  }
  
}
