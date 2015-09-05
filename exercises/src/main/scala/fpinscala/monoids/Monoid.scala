package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A

  def reversed: Monoid[A] = {
    val p = this
    new Monoid[A] {
      def op(a1: A, a2: A) = p.op(a2, a1)
      def zero = p.zero
    }
  }
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def zero = 0
    def op(i1: Int, i2: Int) = i1 + i2
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def zero = 1
    def op(i1: Int, i2: Int) = i1 * i2
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def zero = false
    def op(b1: Boolean, b2: Boolean) = b1 || b2
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def zero = true
    def op(b1: Boolean, b2: Boolean) = b1 && b2
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero = None
    def op(o1: Option[A], o2: Option[A]) = o1.orElse(o2)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def zero = identity
    def op(e1: A=>A, e2: A=>A) = e1 andThen e2
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = sys.error("todo")

  def stringMonoid(s: String): Monoid[String] = new Monoid[String] {
    def zero = ""
    def op(s1: String, s2: String) = s1 + s2
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    foldMap(as, m)(identity)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero) { (result, i) => m.op(result, f(i)) }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B].reversed)(a => b => f(a, b))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.zero
      case 1 => f(as(0))
      case n => {
        val (l, r) = as.splitAt(n / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
    }

  def ordered(ints: IndexedSeq[Int]): Boolean = {

    sealed abstract class OrderTracking {
      def isOrdered: Boolean
    }
    case class OrderedPortion(min: Int, max: Int) extends OrderTracking {
      def isOrdered = true
    }
    case object EmptyPortion extends OrderTracking {
      def isOrdered = true
    }
    case object UnorderedPortion extends OrderTracking {
      def isOrdered = false
    }

    val m = new Monoid[OrderTracking] {
      def zero = EmptyPortion
      def op(o1: OrderTracking, o2: OrderTracking) = (o1, o2) match {
        case (op1: OrderedPortion, op2: OrderedPortion) if op2.min >= op1.max => OrderedPortion(op1.min, op2.max)
        case (op1: OrderedPortion, EmptyPortion) => op1
        case (EmptyPortion, op2: OrderedPortion) => op2
        case _ => UnorderedPortion
      }
    }

    foldMapV(ints, m)(i => OrderedPortion(i, i)).isOrdered
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def zero = Par.unit(m.zero)
    def op(p1: Par[A], p2: Par[A]) = p1.zip(p2).map { case (a1, a2) => m.op(a1, a2) }
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    foldMapV(v, par(m)) { a => Par.fork(Par.unit(f(a))) }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero = Stub("")
    def op(wc1: WC, wc2: WC) = (wc1, wc2) match {
      case (p1: Part, p2: Part) => Part(p1.lStub, p1.words + p2.words + (if ((p1.rStub + p2.lStub).isEmpty) 0 else 1), p2.rStub)
      case (s: Stub, p: Part) => Part(s.chars + p.lStub, p.words, p.rStub)
      case (p: Part, s: Stub) => Part(p.lStub, p.words, p.rStub + s.chars)
      case (s1: Stub, s2: Stub) => Stub(s1.chars + s2.chars)
    }
  }

  def count(s: String): Int = {

    def swc(s: String) =
      if (s.isEmpty) 0 else 1

    def count(wc: WC) = wc match {
      case Stub(s) => swc(s)
      case Part(l, words, r) => swc(l) + words + swc(r)
    }

    def toWC(s: String): WC =
      foldMapV(s.toIndexedSeq, wcMonoid) { c => Stub(c.toString) }

    count(toWC(s))
  }


  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero) { (b, a) => mb.op(b, f(a)) }

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)(identity)(m)

  def toList[A](as: F[A]): List[A] =
    foldMap(as)(List(_))(listMonoid[A])
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

