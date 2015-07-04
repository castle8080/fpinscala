package fpinscala.parallelism

import java.util.concurrent.Executors

object ParTest {
  
  val es = Executors.newCachedThreadPool

  def ex7_3 = {
    
    val p1 = Par.fork {
      Thread.sleep(500)
      Par.unit(8)
    }
    
    val p2 = Par.fork {
      Thread.sleep(500)
      Par.unit(7)
    }
    
    val p3 = Par.myMap2(p1, p2) { _ * _ }
    
    val start = System.currentTimeMillis
    val result = p3(es).get
    val ec = System.currentTimeMillis - start
    
    println(s"Result: $result in $ec ms.")
    
  }
  
  def main(args: Array[String]): Unit = {
    ex7_3
  }
  
}