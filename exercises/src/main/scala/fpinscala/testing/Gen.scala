package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  
  def check: Boolean
  
  def &&(p: Prop): Prop = {
    new Prop {
      def check: Boolean = Prop.this.check && p.check
    }
  }
  
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
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

