package fpinscala.testing

import fpinscala.state._

object Exercises_GenFirstDraft {
  
  def main(args: Array[String]): Unit = {
    println(Gen.listOfN(10, Gen.boolean).sample.run(new RNG.Simple(System.currentTimeMillis)))
    println(Gen.listOfN(10, Gen.choose(1, 3)).sample.run(new RNG.Simple(System.currentTimeMillis)))  
  }

}