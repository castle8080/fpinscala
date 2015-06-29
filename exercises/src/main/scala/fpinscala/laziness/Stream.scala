package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList(): List[A] =
    foldRight(List.empty[A]) { (i, acc) => i :: acc }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  def take(n: Int): Stream[A] = this match {
    case _ if (n <= 0) => empty[A]
    case Empty         => empty[A]
    case Cons(h, t)    => cons(h(), t().take(n-1))
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => empty[A]
  }

  def forAll(p: A => Boolean): Boolean =
    find(!p(_)).isEmpty

  // This is the answer the book had.
  // I was really confused how it stopped evaluation early.
  // The foldRight implementation for streams is pretty awesome.
    
  /*
   * Since `&&` is non-strict in its second argument, this terminates the traversal as soon as a nonmatching element is found.
   */
  def forAllBook(f: A => Boolean): Boolean =
    foldRight(true)((a,b) => f(a) && b)
  
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, rest) =>
      if (p(h))
        cons(h, rest.takeWhile2(p))
      else
        empty
    }
    
  def headOption: Option[A] = 
    foldRight(Option.empty[A]) { (h, _) => Some(h) }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (h, remaining) => cons(f(h), { remaining }) }
  
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, remaining) =>
      if (f(h))
        cons(h, remaining)
      else
        remaining
    }
  
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s) { (h, remaining) => cons(h, remaining) }
  
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (h, remaining) => f(h).append(remaining) }
  
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}