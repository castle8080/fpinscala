package fpinscala.testing

import fpinscala.state._

object SGenTest {

  def get[A](g: Gen[A]) = GenTest.Runner.run(g)
  
  def ex_8_10 = {
    val sg = Gen.boolean.unsized
    println(get(sg.forSize(8)))
  }

  def ex_8_12 = {
    val sg = Gen.listOf(Gen.boolean)
    val values = get(sg.forSize(10))
    println(values)
  }
  
  def main(args: Array[String]): Unit = {
    ex_8_10
    ex_8_12
  }

}