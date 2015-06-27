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
    
  }
  
}