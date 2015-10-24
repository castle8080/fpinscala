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
  
}