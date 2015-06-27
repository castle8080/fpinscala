package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

object OptionTest {

  def main(args: Array[String]): Unit = {
    
    val noInt: Option[Int] = None
    
    println(Some(1).map(_ + 1))
    println(noInt.map(_ + 1))
    
    println(Some('a').getOrElse('z'))
    println((None: Option[Char]).getOrElse('z'))
    
    println(Some(1).flatMap(a => Some(2).map(b => a + b)))
    println(noInt.flatMap(a => Some(2).map(b => a + b)))
    println(Some(1).flatMap(a => noInt.map(b => a + b)))
    
    println(Some(1).orElse(Some(2)))
    println(noInt.orElse(Some(4)))
    
    println(Some(1).filter { _ > 4})
    println(Some(10).filter { _ > 4})
    
    println(Option.variance(List(1.0, 2.0, 7.7, 4.5)))
    
    println(Option.map2(Some(1), Some(2)) { _+ _ })
    
    println(Option.sequence(List(Some(1), Some(2))))
    
    println(Option.sequence2(List(Some(1), Some(2))))
    
  }
  
}