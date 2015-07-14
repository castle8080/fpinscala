package fpinscala.testing

import fpinscala.state._

object Exercises_GenFirstDraft {
  
  case class Gen[A](sample: State[RNG,A])
  
  object Gen {
    
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen { State {
      RNG.map(RNG.nonNegativeLessThan(stopExclusive - start)) { _ + start }
    }}

    def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))
    
    def boolean: Gen[Boolean] = Gen { State {
      RNG.map(RNG.nonNegativeLessThan(2)) { _ == 0 }
    }}
    
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen { State.sequence((1 to n).map { n => g.sample }.toList) }

  } 
  
  def main(args: Array[String]): Unit = {
    println(Gen.listOfN(10, Gen.boolean).sample.run(new RNG.Simple(System.currentTimeMillis)))
    println(Gen.listOfN(10, Gen.choose(1, 3)).sample.run(new RNG.Simple(System.currentTimeMillis)))  
  }

}