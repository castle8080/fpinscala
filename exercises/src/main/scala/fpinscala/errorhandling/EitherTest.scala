package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

object EitherTest {
  
  def main(args: Array[String]): Unit = {
    
    println({
      val item: Either[String, Int] = Left("Oh nose")
      item.map(i => i * 2)
    })
    
    println({
      val item: Either[String, Int] = Right(2)
      item.map(i => i * 2)
    })
    
    {
      def flatMapTest(item1: Either[String, Int], item2: Either[String, Int]): Unit = {
        println(item1.flatMap { i1 => item2.map { i2 => i1 + i2 } })
      }
      
      flatMapTest(Right(1), Right(2))
      flatMapTest(Left("#1 is no good."), Right(2))
      flatMapTest(Right(1), Left("#2 failed!"))
      flatMapTest(Left("Boom!"), Left("Project #1 failed before me, so I'm good?"))
    }

    println(Right("Extend claws").orElse(Right("Run")))
    println(Left("Hydraulic motor failure").orElse(Right("Headbutt")))
    
    {
      def tryTrick(trick: String, failureMode: String): Either[String, String] =
        if (math.random <= 0.5)
          Right(trick)
        else
          Left(failureMode)
      
      for (n <- 1 to 10)
        println(
          tryTrick("Kickflip", "Biff")
            .map2(tryTrick("Rail slide", "Crunch")) { (t1, t2) =>
              s"$t1 to $t2! Whoa 100 points!"
            })
      
      for (n <- 1 to 10)
        println(Either.sequence(List(tryTrick("Kickflip", "Biff"), tryTrick("Rail slide", "Crunch"))))
      
      for (n <- 1 to 10)
        println(Either.sequenceAll(List(tryTrick("Kickflip", "Biff"), tryTrick("Rail slide", "Crunch"))))
            
    }
    
    
  }
  
}