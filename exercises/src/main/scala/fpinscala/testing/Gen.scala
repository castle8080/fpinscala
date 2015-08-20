package fpinscala.testing

import Gen._
import Prop._
import fpinscala.state._
import fpinscala.laziness.Stream

//case class Prop(run: (MaxSize,TestCases,RNG) => Result)

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
   
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    // It seems like a bug to me that run doesn't return the new RNG?
    Prop.this.run(max, n, rng) match {
      case f: Falsified => f
      case Passed => p.run(max, n, rng)
    }
  }
  
  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    Prop.this.run(max, n, rng) match {
      case Passed => Passed
      case f: Falsified => p.run(max, n, rng)
    }
  }
}

object Prop {

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max,n,rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    
    val prop: Prop = props.map(p => Prop { (max, _, rng) =>
      p.run(max, casesPerSize, rng)
    }).toList.reduce(_ && _)
    
    prop.run(max,n,rng)
  }
  
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val results = randomStream(as)(rng).zip(Stream.from(0)).take(n).map { case (a, i) =>
      try {
        if (f(a))
          Passed : Result
        else
          Falsified(a.toString, i) : Result
      }
      catch {
        case e: Exception => Falsified(buildMsg(a, e), i) : Result
      }
    }
    
    results.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
  {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
  }
}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))
  
  def int: Gen[Int] =
    Gen(State(RNG.int))
    
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNeagativeBetween(start, stopExclusive)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))
    
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    sequence(List.fill(n)(g))

  def sequence[A](gens: List[Gen[A]]): Gen[List[A]] =
    Gen(State.sequence(gens.map(_.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)
    
  def normal: Gen[Double] =
    Gen(State(RNG.double))
    
  def weighted[A](gens: (Gen[A],Double)*): Gen[A] = {
    val starts = gens.scanLeft(0.0)(_ + _._2)
    val total = starts.last.toDouble
    val weightedRanges = gens.zip(starts).map { case (gen, start) =>
      (gen._1, start.toDouble / total, (start + gen._2) / total)
    }
    
    normal.flatMap { d =>
      weightedRanges.find(entry => entry._2 <= d && entry._3 > d).get._1
    }
  }
  
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(Gen.listOfN(_, g))
}

case class Gen[+A](sample: State[RNG,A]) {
  
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))
  
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))
    
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)
    
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN)

  def unsized: SGen[A] =
    SGen(_ => this)  
}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { (forSize) => Gen.listOfN(forSize, g) }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen { (forSize) => Gen.listOfN(List(forSize, 1).max, g) }
  
}

case class SGen[+A](forSize: Int => Gen[A]) {

  def apply(n: Int): Gen[A] = forSize(n)
  
  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))
  
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f))
    
}

