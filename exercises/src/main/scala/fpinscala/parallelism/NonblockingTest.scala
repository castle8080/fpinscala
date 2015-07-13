package fpinscala.parallelism

import java.util.concurrent.Executors

object NonblockingTest {
  
  import Nonblocking._
  
  val es = Executors.newCachedThreadPool

  def ex_7_11 = {
    val p = Par.choiceN(Par.unit(1))(List(
      Par.unit(87),
      Par.unit(99)
    ))
    
    println(Par.run(es)(p))
    
    val p2 = Par.choiceViaChoiceN(Par.unit(false))(Par.unit(41), Par.unit(42))
    
    println(Par.run(es)(p2))
  }
  
  def main(args: Array[String]): Unit = {
    ex_7_11
  }
  
  
}