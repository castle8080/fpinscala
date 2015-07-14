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

}

case class Gen[+A](sample: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

