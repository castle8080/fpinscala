package fpinscala.parallelism

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

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
  
  def ex7_4 = {
    
    def waitAndGive[A](a: A): A = {
      Thread.sleep(100)
      a
    }
    
    val asyncWaitAndGive = Par.asyncF(waitAndGive[Int])
    
    println(asyncWaitAndGive(8)(es).get)
  }
  
  def ex7_5 = {
    def waitAndGive[A](a: A): A = {
      Thread.sleep(100)
      a
    }
    
    val pars =
      (  for {
           x <- 1 to 10
         } yield Par.lazyUnit(waitAndGive(x))
      ).toList
      
    val pList = Par.sequence(pars)
    
    val start = System.currentTimeMillis
    val results = pList(es).get
    val ec = System.currentTimeMillis - start
    
    println(s"Received [$results] in $ec ms.")
  }
  
  def main(args: Array[String]): Unit = {
    ex7_3
    ex7_4
    ex7_5
  }
  
}