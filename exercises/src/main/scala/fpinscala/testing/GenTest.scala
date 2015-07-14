package fpinscala.testing

import fpinscala.state._

object Exercises_GenFirstDraft {
  
  def get[A](g: Gen[A]) = {
    g.sample.run(new RNG.Simple(System.currentTimeMillis))
  }
  
  def ex_8_5 = {
    println(get(Gen.listOfN(10, Gen.boolean)))
    println(get(Gen.listOfN(10, Gen.choose(1, 3)))) 
  }
  
  def ex_8_6 = {
    println(get(Gen.choose(5, 10).listOfN(Gen.unit(5)))) 
  }
  
  def ex_8_7 = {
    println(get(Gen.union(Gen.unit(42), Gen.unit(314)).listOfN(10)))
  }
  
  def main(args: Array[String]): Unit = { 
    ex_8_5
    ex_8_6
    ex_8_7
  }

}