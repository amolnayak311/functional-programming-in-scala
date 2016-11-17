package chapter3

object Exercise3_26 {
  
  def maxElem(tree: Tree[Int]): Int = tree match {
    case Leaf(n) => n
    case Branch(left, right) => maxElem(left) max maxElem(right)
    case Empty() => throw new UnsupportedOperationException("Empty node not allowed")
  }
  
  
  def main(args: Array[String]): Unit = {
    assert(maxElem(Leaf(3)) == 3)
    assert(maxElem(Branch(Leaf(3), Leaf(10))) == 10)
    println("All tests successful")
  }
}