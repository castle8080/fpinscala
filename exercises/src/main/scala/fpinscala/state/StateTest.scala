package fpinscala.state

object StateTest {
  
  // For testing.
  
  // This is not functional
  def newRNG = new RNG.Simple(System.currentTimeMillis)
  
  def rngStream[A](rng: RNG, f: RNG => (A, RNG)): Stream[A] = {
    val (a, nRng) = f(rng)
    Stream.cons(a, rngStream(nRng, f))
  }
  
  def ex6_1() = {
    println(rngStream(newRNG, RNG.nonNegativeInt).take(10).toList)
  }
  
  def ex6_2() = {
    println(rngStream(newRNG, RNG.double).take(10).toList)
        
  }
  
  def ex6_3() = {
    println(RNG.intDouble(newRNG))
    println(RNG.doubleInt(newRNG))
    println(RNG.double3(newRNG))
  }
  
  def ex6_4() = {
    println(RNG.ints(10)(newRNG))
  }
  
  def ex6_5() = {
    println(RNG.double2(newRNG))
  }
  
  def ex6_6() = {
    println(RNG.doubleDouble2(newRNG))
  }
  
  def ex6_7() = {
    val ds = RNG.sequence(List[RNG.Rand[Double]](
      RNG.double,
      RNG.double,
      RNG.double
    ))
    
    println(ds(newRNG))
  }
  
  def ex6_8() = {
    println(RNG.nonNegativeLessThan(10000)(newRNG))
  }
  
  def ex6_9() = {
    val dieRoll = RNG.mapViaFlatMap(RNG.nonNegativeLessThan(6)) { x => x + 1 }
    val diceRoll = RNG.map2ViaFlatMap(dieRoll, dieRoll)((a,b) => a + b)
    
    println(RNG.sequence(List(dieRoll, dieRoll, dieRoll))(newRNG))
    println(RNG.sequence(List(diceRoll, diceRoll, diceRoll))(newRNG))
  }
  
  def ex6_10() = {
    
    println(State.unit(1).run("Bob"))
    
    println(State
      .unit[String, Int](1)
      .map { x => x + 1 }
      .run("Bob"))

    val s1 = State.unit[String, Int](1)
    val s2 = State.unit[String, Int](2)
        
    println(s1.flatMap { x => s2 }.run("Bob"))
    println(s1.map2(s2) { (a, b) => a + b }.run("Joe"))
  }
  
  def main(args: Array[String]): Unit = {
    ex6_1()
    ex6_2()
    ex6_3()
    ex6_4()
    ex6_5()
    ex6_6()
    ex6_7()
    ex6_8()
    ex6_9()
    ex6_10()
  }
  
}