package fpinscala.monoids

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, Checkers}
import org.scalatest.{Matchers, FlatSpec}

class MonoidSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  it should "adhere to ordered properties" in {
    forAll { (ints: Vector[Int]) =>
      if (ints.isEmpty)
        Monoid.ordered(ints) shouldBe true
      else {
        Monoid.ordered(ints) shouldBe ints.zip(ints.tail).foldLeft(true) { case (ordered, (i1, i2)) => ordered && i1 <= i2 }
        Monoid.ordered(ints.sorted) shouldBe true
      }
    }
  }
}
