package fpinscala.datastructures

object TreeTest {

  import Tree._
  
  val testTree =
    Branch(
      Branch(
        Leaf(88),
        Leaf(-1)),
      Branch(
        Leaf(2),
        Branch(
          Leaf(3),
          Leaf(5))))
        
  def main(args: Array[String]) = {
    
    println(s"Size of testTree: ${size(testTree)}")
    println(s"Maximum of testTree: ${maximum(testTree)}")
    println(s"Depth of testTree: ${depth(testTree)}")
    
    val treeAdd1 = map(testTree)(_ + 1)
    
    println(s"mapped test tree: $treeAdd1")
    
    println(s"Size of testTree: ${size2(testTree)}")
    println(s"Maximum of testTree: ${maximum2(testTree)}")
    println(s"Depth of testTree: ${depth2(testTree)}")
    
    val treeAdd12 = map2(testTree)(_ + 1)
    
    println(s"mapped test tree: $treeAdd12")
        
  }
          
}