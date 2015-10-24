package fpinscala.parallelism

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future

object ParTest {
  
  val es = Executors.newCachedThreadPool

  def timedRun[A](run: => A): (A, Long) = {
    val start = System.currentTimeMillis
    val a = run
    val ec = System.currentTimeMillis - start
    (a, ec)
  }
  
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
    
    val (result, ec) = timedRun(p3(es).get)
    
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
    
    {
      val pars =
        (  for {
             x <- 1 to 10
           } yield Par.lazyUnit(waitAndGive(x))
        ).toList
        
      val pList = Par.sequence(pars)
      val (results, ec) = timedRun(pList(es).get)
      println(s"Received [$results] in $ec ms.")
    }
    
    {
      val (results, ec) = timedRun(Par.parMap((1 to 20).toList)(waitAndGive)(es).get)  
      println(s"Received [$results] in $ec ms.")
    }
  }
  
  def ex7_6 = {
    def longFilter(i: Int): Boolean = {
      Thread.sleep(100)
      i % 2 == 1
    }
    
    val (results, ec) = timedRun(Par.parFilter((1 to 30).toList)(longFilter)(es).get)
    println(s"Received [$results] in $ec ms.")
  }
  
  def ex7_6_extra = {
    println(Par.parMax(1 to 5000)((a, b) => a - b)(es).get)
  }
  
  def testParFlatmap = {
    val pn = Par.flatMap(Par.unit(9))(n => Par.unit(9 + 1))
    println(pn(es).get)
  }

  def main(args: Array[String]): Unit = {
    ex7_3
    ex7_4
    ex7_5
    ex7_6
    ex7_6_extra
    testParFlatmap
    System.exit(0)
  }
  
}