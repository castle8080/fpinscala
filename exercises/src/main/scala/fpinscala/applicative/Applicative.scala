package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._

trait Applicative[F[_]] extends Functor[F] {
    
  def unit[A](a: => A): F[A]
  
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D]) (f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa) { _ apply _ }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, mlb) =>
      map2(f(a), mlb) { _ :: _ }
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // The book I have calls this method product?
  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb) { (_,_) }

  def product[G[_]](G: Applicative[G]) = {
    type Product[x] = (F[x], G[x])
    val F = this
    
    new Applicative[Product] {
    
      def unit[A](a: => A): Product[A] =
        (F.unit(a), G.unit(a))
    
      override def map2[A,B,C](fa: Product[A], fb: Product[B])(f: (A, B) => C): Product[C] = {
        val fab = F.map2(fa._1, fb._1) { f(_,_) }
        val gab = G.map2(fa._2, fb._2) { f(_,_) }
        
        (fab, gab)
      }
    }
  }
    

  def compose[G[_]](G: Applicative[G]) = {
    type Composed[x] = F[G[x]]
    val F = this
    
    new Applicative[Composed] {
      
      def unit[A](a: => A): Composed[A] =
        F.unit(G.unit(a))
        
      override def map2[A,B,C](ca: Composed[A], cb: Composed[B])(f: (A, B) => C): Composed[C] =
        F.map2(ca, cb) { (ga, gb) => G.map2(ga, gb)(f) }
    }
  }
  
  
  //: Applicative[({type f[x] = F[G[x]]})#f] = ???

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ???
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    
    def unit[A](a: => A): Either[E,A] =
      Right(a)
    
    override def flatMap[A,B](e: Either[E,A])(f: A => Either[E,B]): Either[E,B] = e match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    
    def unit[A](a: => A): Validation[E,A] =
      Success(a)
  
    override def map2[A,B,C](va: Validation[E,A], vb: Validation[E,B])(f: (A, B) => C): Validation[E,C] =
      (va, vb) match {
        case (fa: Failure[E], fb: Failure[E]) =>
          Failure[E](fb.head, (fb.tail :+ fa.head) ++ fa.tail)
        case (fa: Failure[E], _) =>
          fa
        case (_, fb: Failure[E]) =>
          fb
        case (sa: Success[A], sb: Success[B]) =>
          Success(f(sa.a, sb.a))
      }

  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = ???

  val optionTraverse = ???

  val treeTraverse = ???
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  // I put these in State
  
  // def get[S]: State[S, S] =
  //   State(s => (s, s))

  //def set[S](s: S): State[S, Unit] =
  //  State(_ => ((), s))
}
