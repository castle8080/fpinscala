package fpinscala.laziness

object StreamTest {

  
  
  def main(args: Array[String]): Unit = {
    
    val xs = Stream(1,2,3)
    
    println(xs)
    println(xs.toList())
   
    println(xs.take(1).toList)
    println(xs.drop(1).toList)
    
    println(Stream.empty[Int].drop(10).toList)
    println(Stream(1,2,3,4,5).takeWhile(_ < 3).toList())
    
    println(Stream(1,2,3,4).forAll(_ < 10))
    println(Stream(1,2,3,4).forAll(_ < 4))
    println(Stream(1,2,3,4).forAll(_ < 1))
    println(Stream.empty[Int].forAll(_ < 1))
    
    
    {
      
      def notifyingStream[A](s: Stream[A]): Stream[A] = s match {
        case Empty => s
        case Cons(h, t) => Stream.cons({
          val _h = h()
          println("Producing: " + _h)
          _h
        }, t())
      }
      
      val s = notifyingStream(Stream(1,2,3,4,5))
      println("Have a stream")
      
      println(s.forAll { x => x > 1 })
      println(s.forAllBook { x => x > 1 })
    }
    
    
    println(Stream(1,2,3,4).takeWhile2(_ < 2).toList)
    println(Stream(1,2).headOption)
    println(Stream.empty[Int].headOption)
    
  }
  
}