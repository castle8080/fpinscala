package fpinscala

import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.FlatSpec

trait SpecBase extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  
}