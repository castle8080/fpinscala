package fpinscala.testing

import fpinscala.state._

object GenTest {
  
  /**
   * Hack for testing
   */
  object Runner {

    private var rng: RNG = new RNG.Simple(System.currentTimeMillis)
    
    def run[A](g: Gen[A]): A = {
      val (result, newRng) = g.sample.run(rng)
      rng = newRng
      result
    }
    
  }
  
  def get[A](g: Gen[A]) = Runner.run(g)
  
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
  
  def ex_8_8 = {
    
    def runWeightedGen[A](gen: Gen[A]): Unit = {
      val values = get(Gen.listOfN(1000, gen))
      val size = values.length
      val valRatios = values
        .groupBy(identity)
        .mapValues(items => items.length / size.toDouble)
        
      for ((v, ratio) <- valRatios)
        println(s"[$v] -> $ratio")
    }
    
    runWeightedGen(
      Gen.weighted(
        (Gen.unit(1), 1)
      )
    )
    
    runWeightedGen(
      Gen.weighted(
        (Gen.unit(1), 1),
        (Gen.unit(2), 2)
      )
    )
    
    runWeightedGen(
      Gen.weighted(
        (Gen.unit(1), 1),
        (Gen.unit(2), 2),
        (Gen.unit(3), 3),
        (Gen.unit(4), 4)
      )
    )
  }
  
  def ex_8_9 = {
    
    def runProp(p: Prop) = {
      println(p.run(1, RNG.Simple(System.currentTimeMillis())))
    }
    
    runProp(Prop { (n, rng) => Prop.Passed } && Prop { (n, rng) => Prop.Passed })
    runProp(Prop { (n, rng) => Prop.Passed } && Prop { (n, rng) => Prop.Falsified("foo", 1) })
    runProp(Prop { (n, rng) => Prop.Falsified("bar", 2) } && Prop { (n, rng) => Prop.Passed })
    runProp(Prop { (n, rng) => Prop.Falsified("bar", 2) } && Prop { (n, rng) => Prop.Falsified("foo", 1) })
    
    runProp(Prop { (n, rng) => Prop.Passed } || Prop { (n, rng) => Prop.Passed })
    runProp(Prop { (n, rng) => Prop.Passed } || Prop { (n, rng) => Prop.Falsified("foo", 1) })
    runProp(Prop { (n, rng) => Prop.Falsified("bar", 2) } || Prop { (n, rng) => Prop.Passed })
    runProp(Prop { (n, rng) => Prop.Falsified("bar", 2) } || Prop { (n, rng) => Prop.Falsified("foo", 1) })
  }
  
  def main(args: Array[String]): Unit = { 
    ex_8_5
    ex_8_6
    ex_8_7
    ex_8_8
    ex_8_9
  }

}