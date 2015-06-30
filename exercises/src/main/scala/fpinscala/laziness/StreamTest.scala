package fpinscala.laziness

object StreamTest {

  
  
  def main(args: Array[String]): Unit = {
    
    val xs = Stream(1,2,3)
    
    println(xs)
    println(xs.toList())
   
    println(xs.take(1).toList)
    println(xs.take(2).toList)
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
    
    println(Stream(1,2,3,4).map(_ * 2).toList)
    println(Stream(1,2,3,4,1,1,10).filter(_ > 2).toList)
    println((Stream(1,2,3) append Stream(3,4,5)).toList)
    
    println(Stream(1,2,3).flatMap { x => Stream(x,x) }.toList)
    
    println(Stream.constant(10).take(10).toList)
    println(Stream.from(5).take(10).toList)
    
    println(Stream.fibs().take(10).toList)

    println(Stream.unfold(12){ x =>
      if (x <= 0)     None
      else if (x < 5) Some(x, 0)
      else            Some(5, x-5)
    }.toList)
    
    println(Stream.fibs2.take(10).toList)
    println(Stream.from2(9).take(10).toList)
    println(Stream.constant2(99).take(10).toList)
    println(Stream.ones2.take(10).toList)
    
    println(Stream(1,2,3).mapUnfold { _ * 3 }.toList)
    println(Stream.from(100).takeUnfold(10).toList)
    
    println(Stream.from(1).takeWhile(_ < 100).toList)
    println(Stream.ones.zipWith(Stream(1,2,3))(_ + _).toList)
    
    println(Stream(1,2).zipAll(Stream("Bob", "Jim", "Barb")).toList)
    
  }
  
}