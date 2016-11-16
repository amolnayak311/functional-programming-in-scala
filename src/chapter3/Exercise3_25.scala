package chapter3

import chapter3.Tree._

object Exercise3_25 {
    
  /**
   * Size only counts leaves and branches.Empty child nodes aren't
   * counted
   *  
   */
  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Empty() => 0
    case Branch(l, r) => 1 + size(l) + size(r) 
  }
  
  
  def main(args: Array[String]): Unit = {
    assert(size(Empty()) == 0)
    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size(Branch(Branch(Leaf(1), Empty()), Leaf(2))) == 4)
    println("All tests successful")
  }
}