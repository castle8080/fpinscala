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
      s.split(""" +""").filter(!_.isEmpty).size

    forAll { (s: String) =>
      Monoid.count(s) shouldBe wc2(s)
    }
  }

  "The ListFoldable" should "concatenate" in {

    // Basic example
    ListFoldable.concatenate(List("a", "b", "c"))(Monoid.stringMonoid) shouldBe "abc"

    // Deeper check of strings
    forAll { (s: List[String]) =>
      ListFoldable.concatenate(s)(Monoid.stringMonoid) shouldBe s.mkString("")
    }

    // Deeper check of ints
    forAll { (is: List[Int]) =>
      ListFoldable.concatenate(is)(Monoid.intAddition) shouldBe is.sum
    }
  }

  it should "foldRight" in {
    forAll { (s: List[String]) =>
      val frr = ListFoldable.foldLeft(s)("") { (s, i) => i + s }
      val flr = ListFoldable.foldRight(s.reverse)("") { (i, s) => i + s }
      frr shouldBe flr
    }
  }

  "The IndexedSeqFoldable" should "concatenate" in {

    // Basic example
    IndexedSeqFoldable.concatenate(Vector("a", "b", "c"))(Monoid.stringMonoid) shouldBe "abc"

    // Deeper check of strings
    forAll { (s: Vector[String]) =>
      IndexedSeqFoldable.concatenate(s)(Monoid.stringMonoid) shouldBe s.mkString("")
    }

    // Deeper check of ints
    forAll { (is: Vector[Int]) =>
      IndexedSeqFoldable.concatenate(is)(Monoid.intAddition) shouldBe is.sum
    }
  }

  it should "foldRight" in {
    forAll { (s: Vector[String]) =>
      val frr = IndexedSeqFoldable.foldLeft(s)("") { (s, i) => i + s }
      val flr = IndexedSeqFoldable.foldRight(s.reverse)("") { (i, s) => i + s }
      frr shouldBe flr
    }
  }
}
