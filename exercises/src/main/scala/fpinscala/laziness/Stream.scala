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
  
  /*
   * unfold to implement map , take , takeWhile , zipWith (as in chapter 3), and
zipAll
   */
  
  def mapUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case _ => None
    }
  
  def takeUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if (x > 0) => Some(h(), (t(), x - 1))
      case _ => None
    }
  
  def takeWhileUfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if (p(h())) => Some(h(), t())
      case _ => None
    }
  
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2) { (a,b) => (a,b) }
  
  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) =>
        Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) =>
        Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }
  
  def startsWith[B](s: Stream[B]): Boolean =
    this
      .zipAll(s)
      .takeWhile(e => e._2.isDefined)
      .forAll(e => e._1 == e._2)
    
  def tails: Stream[Stream[A]] =
    unfold(this) { s =>
      s match {
        case (Cons(h, t)) => Some(s, t())
        case _ => None
      }
    }.append(Stream(empty))
    
    
  def scanRight[B](initial: B)(f: (A,B) => B): Stream[B] =
    foldRight((Stream(initial), initial)) { case (item, (s, last)) =>
      val current = f(item, last)
      (cons(current, s), current)
    }._1
    
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

  def fibs():Stream[Int] = {
    def fibsPair(pair: (Int, Int)): Stream[(Int, Int)] =
      cons(pair, fibsPair(pair._2, pair._1 + pair._2))
      
    fibsPair(0, 1).map(_._1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z)
      .map { case (item, newState) => cons(item, unfold(newState)(f)) }
      .getOrElse(empty[A])
      
  def fibs2(): Stream[Int] =
    unfold((0, 1))(pair => Some(pair._1, (pair._2, pair._1 + pair._2)))
    
  def from2(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x+1))
  
  def constant2[A](a: A): Stream[A] =
    unfold(a)(a => Some((a, a)))
   
  def ones2 = unfold(1)(a => Some((a, a)))
    
}