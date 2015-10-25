package fpinscala.monads

import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MonadSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  
  import Monad._
  
  "A ListMonad" should "map things" in {
    forAll { (values: List[Int], increment: Int) =>
      (listMonad.map(values)(_ + increment)) shouldBe values.map(_ + increment)
    }
  }
  
  "An Option Monad" should "do sequence" in {
    optionMonad.sequence(List(Option(1))) shouldBe Some(List(1))
    optionMonad.sequence(List(Option(1), Option(2))) shouldBe Some(List(1, 2))
    optionMonad.sequence(List.empty) shouldBe Some(List.empty)
    optionMonad.sequence(List(Option(1), None)) shouldBe None
    optionMonad.sequence(List(None)) shouldBe None
  }
  
  "An Id Monad" should "by mappable" in {
    idMonad.map(Id(1))(_ + 1) shouldBe Id(2)
  }

}