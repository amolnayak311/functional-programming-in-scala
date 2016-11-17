package chapter3

object Exercise3_27 {
  
  def maxDepth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) | Empty() => 0
    case Branch(left, right) => 1 + (maxDepth(left) max maxDepth(right))
  }
  
  def main(args: Array[String]): Unit = {
    assert(maxDepth(Leaf(1)) == 0)
    assert(maxDepth(Branch(Leaf(1), Leaf(2))) == 1)
    println("All tests successful")
  }
  
}