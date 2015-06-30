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
  
  def main(args: Array[String]): Unit = {
    ex6_1()
    ex6_2()
  }
  
}