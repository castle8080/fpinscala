package fpinscala.monoids

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, Checkers}
import org.scalatest.{Matchers, FlatSpec}

class MonoidSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "The ordered monoid" should "adhere to ordered properties" in {
    forAll { (ints: Vector[Int]) =>
      if (ints.isEmpty)
        Monoid.ordered(ints) shouldBe true
      else {
        Monoid.ordered(ints) shouldBe ints.zip(ints.tail).foldLeft(true) { case (ordered, (i1, i2)) => ordered && i1 <= i2 }
        Monoid.ordered(ints.sorted) shouldBe true
      }
    }
  }

  "Word count" should "find the right word count" in {
    def wc2(s: String) =
      s.split("""\s+""").filter(!_.isEmpty).size

    forAll { (s: String) =>
      Monoid.count(s) shouldBe wc2(s)
    }
  }

}
