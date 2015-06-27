package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(value) => Right(f(value))
   case l: Left[E] => l
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(value) => f(value)
   case l: Left[E] => l
 }
   
 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case r: Right[A] => r
   case l: Left[E]  => b
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   for (i1 <- this; i2 <- b) yield f(i1, i2)
}

case class Left[+E](get: E) extends Either[E,Nothing]

case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil): Either[E, List[B]]) { (item, acc) =>
      f(item).map2(acc) { (pItem, results) =>
        pItem :: results
      }
    }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  // Used to accumulate all errors.
  // This could be generalized further though.
  // Perhaps a foldAll?
  def sequenceAll[E,A](es: List[Either[E,A]]): Either[List[E],List[A]] =
    es.foldRight(Right(Nil): Either[List[E], List[A]]) { (item, acc) =>
      (item, acc) match {
        case (Left(error), Left(errors)) => Left(error :: errors)
        case (Left(error), Right(value)) => Left(List(error))
        case (Right(v), Right(values))   => Right(v :: values)
        case (Right(v), Left(errors))    => Left(errors)
      }
    }
    
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}