package fpinscala.testing

import Gen._
import Prop._
import fpinscala.state._
import fpinscala.laziness.Stream

case class Prop(run: (TestCases, RNG) => Result)

object Prop {

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
  
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
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

}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))
  
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
    
}

trait SGen[+A] {

}

